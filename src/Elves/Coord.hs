{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiWayIf        #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE ConstraintKinds      #-}
{-# LANGUAGE ConstrainedClassMethods      #-}

module Elves.Coord where

import           Test.QuickCheck hiding (within)

import qualified Data.Foldable as F
import           Data.Bifunctor (bimap)
import           Data.Monoid (Endo(..))
import           Control.Lens    hiding (contains, index)
import           Data.Ix         (Ix)
import qualified Data.Ix         as Ix
import qualified Data.List       as L

type Bounds i = (i,i)

type Accessor a b = ReifiedLens a a b b

data Heuristic
  = Euclidean
  | Manhattan
  deriving (Show, Eq, Bounded, Enum)

instance Arbitrary Heuristic where
  arbitrary = arbitraryBoundedEnum

measure :: (Coord a, Real (Dimension a)) => Heuristic -> a -> a -> Double
measure Euclidean = straightLine
measure Manhattan = (realToFrac .) . manhattan

magnitude :: (Coord a, Real (Dimension a)) => a -> Double
magnitude = straightLine origin

newtype RealPoint a = RealPoint { realPoint :: a } deriving (Show, Eq, Ord, Ix)

realIso :: Iso (RealPoint a) (RealPoint a) a a
realIso = iso realPoint RealPoint

class Ord a => Coord a where
  type Dimension a :: *
  dimensions :: [Accessor a (Dimension a)]
  origin :: a

  {-# INLINE manhattan #-}
  manhattan :: (Num (Dimension a)) => a -> a -> Dimension a
  manhattan a b = sum . fmap abs $ zipWith subtract (points a) (points b)

  {-# INLINE points #-}
  points :: a -> [Dimension a]
  points c = [ c ^. d | Lens d <- dimensions ]

  {-# INLINE closestPoint #-}
  closestPoint :: Ord (Dimension a) => a -> (a,a) -> a
  closestPoint _ (lb,ub) | lb == ub = lb
  closestPoint c (lb,ub) = let go dim = let read = view (runLens dim)
                                            h = read ub
                                            l = read lb
                                            x = read c
                                         in Endo $ set (runLens dim) (clamp l h x)
                            in appEndo (foldMap go dimensions) c

{-# INLINE clamp #-}
clamp :: Ord a => a -> a -> a -> a
clamp l _ x | x < l = l
clamp _ h x | x > h = h
clamp _ _ x = x

class Num a => Extent a where
  extent :: a -> a -> a
  midpoint :: a -> a -> a
  divideAt :: (a,a) -> a -> ((a,a), (a,a))

instance Extent Int where
  extent a b = (b - a) + 1
  midpoint a b = (a + b) `div` 2
  divideAt (lb,ub) mp = ((lb, max lb (mp - 1)), (min ub mp, ub))

instance Extent Double where
  extent a b = b - a
  midpoint a b = (a + b) / 2
  divideAt (lb,ub) mp = ((lb, max lb (mp - 0.000001)), (min ub mp, ub))

instance Coord Int where
  type Dimension Int = Int
  dimensions = [Lens (lens id (pure id))]
  origin = 0

instance Coord Double where
  type Dimension Double = Double
  dimensions = [Lens (lens id (pure id))]
  origin = 0

scaleBounds :: Extent a => a -> Bounds a -> Bounds a
scaleBounds factor (lb, ub) = let size = extent lb ub
                                  mid = midpoint lb ub 
                                  diff = midpoint (size * factor) 0
                               in (mid - diff, mid + diff)

intLens :: (Integral a) => Lens' s a -> Accessor s Int
intLens l = Lens $ lens (fromIntegral . view l) (\s i -> set l (fromIntegral i) s)

dblLens :: (Real a, Fractional a) => Lens' s a -> Accessor s Double
dblLens l = Lens $ lens (realToFrac . view l) (\s i -> set l (realToFrac i) s)

instance (Integral a, Integral b) => Coord (a,b) where
  type Dimension (a,b) = Int
  dimensions = [intLens _1, intLens _2]
  origin = (0,0)
  
  {-# INLINE manhattan #-}
  manhattan (x0,y0) (x1,y1) = fromIntegral (abs (x0 - x1)) + fromIntegral (abs (y0 - y1))

  {-# INLINE points #-}
  points (x,y) = [ fromIntegral x, fromIntegral y ]

  {-# INLINE closestPoint #-}
  closestPoint p (lb,ub) = if lb == ub
                           then lb
                           else bimap (clamp (fst lb) (fst ub)) (clamp (snd lb) (snd ub)) p

instance (Real a, Fractional a, Real b, Fractional b) => Coord (RealPoint (a,b)) where
  type Dimension (RealPoint (a,b)) = Double
  dimensions = [dblLens (realIso . _1), dblLens (realIso . _2) ]
  origin = RealPoint (0.0,0.0)

instance (Integral a, Integral b, Integral c) => Coord (a,b,c) where
  type Dimension (a,b,c) = Int
  dimensions = [ intLens _1
               , intLens _2
               , intLens _3
               ]
  origin = (0,0,0)

  {-# INLINE manhattan #-}
  manhattan (x0,y0,z0) (x1,y1,z1) = fromIntegral (abs (x0 - x1))
                                  + fromIntegral (abs (y0 - y1))
                                  + fromIntegral (abs (z0 - z1))

  {-# INLINE points #-}
  points (x,y,z) = [ fromIntegral x, fromIntegral y, fromIntegral z ]

  {-# INLINE closestPoint #-}
  closestPoint (x,y,z) ((x0,y0,z0),(x1,y1,z1))
    = (clamp x0 x1 x, clamp y0 y1 y, clamp z0 z1 z)

instance (Real a, Fractional a, Real b, Fractional b, Real c, Fractional c) => Coord (RealPoint (a,b,c)) where
  type Dimension (RealPoint (a,b,c)) = Double
  dimensions = [ dblLens (realIso . _1)
               , dblLens (realIso . _2)
               , dblLens (realIso . _3)
               ]
  origin = RealPoint (0.0,0.0,0.0)

instance (Integral a, Integral b, Integral c, Integral d) => Coord (a,b,c,d) where
  type Dimension (a,b,c,d) = Int
  dimensions = [ intLens _1
               , intLens _2
               , intLens _3
               , intLens _4
               ]
  origin = (0,0,0,0)

instance (Real a, Fractional a
         , Real b, Fractional b
         , Real c, Fractional c
         , Real d, Fractional d) => Coord (RealPoint (a,b,c,d)) where
  type Dimension (RealPoint (a,b,c,d)) = Double
  dimensions = [ dblLens (realIso . _1)
               , dblLens (realIso . _2)
               , dblLens (realIso . _3)
               , dblLens (realIso . _4)
               ]
  origin = RealPoint (0.0,0.0,0.0,0.0)

{-# INLINE contains #-}
-- is a entirely within b?
contains :: (Ord (Dimension a), Coord a)
         => Bounds a -> Bounds a -> Bool
contains (lb, ub) b = containsP lb b && containsP ub b

{-# INLINE containsP #-}
-- like Ix.inRange, but also for non-integral values
containsP :: (Ord (Dimension a), Coord a) => a -> Bounds a -> Bool
containsP p (lb, ub) = all f dimensions
  where
    f d = let x = view (runLens d) p
              lb' = view (runLens d) lb
              ub' = view (runLens d) ub
           in lb' <= x && x <= ub'

{-# INLINE corners #-}
corners :: (Coord i) => (i,i) -> [i]
corners (lb,ub) = L.foldl' f [lb] dimensions
  where
    f cs  l = cs >>= \c -> [c, set (runLens l) (view (runLens l) ub) c]

-- not super reliable for floating types
onEdge :: (Eq (Dimension i), Coord i) => (i,i) -> i -> Bool
onEdge (lb,ub) p = any (\l -> let d = view (runLens l)
                                  x = d p
                               in x == d lb || x == d ub)
                       dimensions

type Overlapping a = (Coord a, Ord (Dimension a), Num (Dimension a))

-- do a and b share any points?
overlaps :: (Overlapping i) => Bounds i -> Bounds i -> Bool
overlaps a b = all (== 0) (mindists a b)

-- general purpose function for computing a straight-line distance
-- between two points on a Cartesian plane of an arbitrary number of dimensions
straightLine :: (Real (Dimension c), Coord c, Floating b) => c -> c -> b
straightLine a b = sqrt . sum $ zipWith ((sqr .) . subtract) (points a) (points b)
  where sqr = (^ (2 :: Int)) . realToFrac . abs

{-# INLINE translate #-}
translate :: (Num (Dimension i), Coord i) => i -> i -> i
translate delta pos = L.foldl' (\c d -> c & runLens d %~ (+ view (runLens d) delta)) pos dimensions

{-# INLINE invert #-}
invert :: (Num (Dimension i), Coord i) => i -> i
invert c = L.foldl' (\c d -> c & runLens d %~ negate) c dimensions

{-# INLINE scale #-}
scale :: (Num (Dimension i), Coord i) => Dimension i -> i -> i
scale n c = L.foldl' (\p d -> p & runLens d %~ (* n)) c dimensions

{-# INLINE mindist #-}
mindist :: (Coord c, Real (Dimension c), Ord (Dimension c), Ord b, Floating b) => c -> (c,c) -> b
mindist p box = straightLine p (closestPoint p box)

-- the distances from a to b, along each dimension
mindists :: (Coord c, Ord (Dimension c), Num (Dimension c))
         => Bounds c -> Bounds c -> [Dimension c]
mindists (l0,h0) (l1,h1) = do
  Lens d <- dimensions
  let a_before_b = (h0 ^. d) < (l1 ^. d)
      b_before_a = (h1 ^. d) < (l0 ^. d)
  if | a_before_b -> pure $ abs (l1 ^. d - h0 ^. d)
     | b_before_a -> pure $ abs (l0 ^. d - h1 ^. d)
     | otherwise  -> pure 0

size :: (Coord a, Extent (Dimension a)) => a -> a -> Dimension a
size a b
  = product [extent (a ^. fld) (b ^. fld) | Lens fld <- dimensions]

setDimension :: Coord a => Accessor a (Dimension a) -> Dimension a -> a -> a
setDimension dim = set (runLens dim)

getDimension :: Coord a => Accessor a (Dimension a) -> a -> Dimension a
getDimension dim = view (runLens dim)

-- get the bounding box for a set of coordinates. The list must not be empty
boundingBox :: (Foldable f, Ord (Dimension i), Coord i) => f i -> Bounds i
boundingBox = L.foldl1 expandB . fmap (\c -> (c,c)) . F.toList
    
expandB :: (Ord (Dimension i), Coord i) => Bounds i -> Bounds i -> Bounds i
expandB (i,j) = expand i . expand j

expand :: (Ord (Dimension i), Coord i) => i -> (i,i) -> (i,i)
expand i bs = L.foldl' f bs dimensions
  where f (lb, ub) l = let v = i ^. runLens l
                        in (over (runLens l) (min v) lb, over (runLens l) (max v) ub)

{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE MultiWayIf        #-}

module Elves.Coord where

import Data.Ix (Ix)
import qualified Data.Ix as Ix
import qualified Data.List as L
import           Control.Lens              hiding (contains, index)

type Accessor a b = ReifiedLens a a b b

class Coord a where
  dimensions :: [Accessor a Int]
  origin :: a

instance Coord Int where
  dimensions = [Lens (lens id (pure id))]
  origin = 0

intLens :: (Integral a) => Lens' s a -> Accessor s Int
intLens l = Lens $ lens (fromIntegral . view l) (\s i -> set l (fromIntegral i) s)

instance (Integral a, Integral b) => Coord (a,b) where
  dimensions = [intLens _1, intLens _2]
  origin = (0,0)

instance (Integral a, Integral b, Integral c) => Coord (a,b,c) where
  dimensions = [ intLens _1
               , intLens _2
               , intLens _3
               ]
  origin = (0,0,0)

instance (Integral a, Integral b, Integral c, Integral d) => Coord (a,b,c,d) where
  dimensions = [ intLens _1
               , intLens _2
               , intLens _3
               , intLens _4
               ]
  origin = (0,0,0,0)

manhattan :: Coord i => i -> i -> Int
manhattan a b = sum . fmap abs $ zipWith subtract (points a) (points b)

points :: Coord c => c -> [Int]
points c = [ c ^. d | Lens d <- dimensions ]

-- is a entirely within b?
within :: (Ix i, Coord i) => (i,i) -> (i,i) -> Bool
within a b = all (Ix.inRange b) (corners a)

corners :: (Coord i) => (i,i) -> [i]
corners (lb,ub) = L.foldl' f [lb] dimensions
  where
    f cs  l = cs >>= \c -> [c, set (runLens l) (view (runLens l) ub) c]

-- do a and b share any points?
overlaps :: (Coord i) => (i,i) -> (i,i) -> Bool
overlaps a b = all (== 0) (mindists a b)

-- general purpose function for computing a straight-line distance
-- between two points on a Cartesian plane of an arbitrary number of dimensions
straightLine :: (Coord c, Floating b) => c -> c -> b
straightLine a b
  = sqrt . sum . map ((^ (2 :: Int)) . realToFrac . abs) $ zipWith subtract (points a) (points b)

translate :: Coord i => i -> i -> i
translate delta pos = L.foldl' (\c d -> c & runLens d %~ (+ view (runLens d) delta)) pos dimensions

invert :: Coord i => i -> i
invert c = L.foldl' (\c d -> c & runLens d %~ negate) c dimensions

scale :: Coord i => Int -> i -> i
scale n c = L.foldl' (\p d -> p & runLens d %~ (* n)) c dimensions

mindist :: (Coord c, Ord b, Floating b) => c -> (c,c) -> b
mindist p box = case filter (> 0) (mindists (p,p) box) of
  []  -> 0
  [x] -> realToFrac x
  _   -> minimum (straightLine p <$> corners box)

-- the distances from a to b, along each dimension
mindists :: (Coord c) => (c,c) -> (c,c) -> [Int]
mindists (l0,h0) (l1,h1) = do
  Lens d <- dimensions
  let a_before_b = (h0 ^. d) < (l1 ^. d)
      b_before_a = (h1 ^. d) < (l0 ^. d)
  if | a_before_b -> pure $ abs (l1 ^. d - h0 ^. d)
     | b_before_a -> pure $ abs (l0 ^. d - h1 ^. d)
     | otherwise  -> pure 0


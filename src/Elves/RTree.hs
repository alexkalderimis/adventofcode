{-# LANGUAGE DeriveFunctor       #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Elves.RTree where

import           Control.DeepSeq           (NFData (..))
import           Control.Lens              hiding (contains, index)
import           Data.Foldable             (Foldable (foldMap))
import           Data.Hashable             (Hashable)
import           Data.Heap                 (Entry (..), Heap)
import qualified Data.Heap                 as Heap
import           Data.Ix                   (Ix)
import qualified Data.Ix                   as Ix
import qualified Data.List                 as L
import           Data.List.NonEmpty        (NonEmpty(..))
import qualified Data.List.NonEmpty        as NE
import           Data.Maybe
import           Data.Ord
import           GHC.Generics              (Generic)
import           Test.QuickCheck.Arbitrary (Arbitrary (arbitrary))

import           Elves.Coord

data RTree i a = Tip | Leaf i a | Region (i,i) (NonEmpty (RTree i a))
             deriving (Show, Ord, Eq, Functor, Generic)

instance (Arbitrary i, Arbitrary a, Ix i, Coord i) => Arbitrary (RTree i a) where
  arbitrary = fmap index arbitrary

instance (Hashable i, Hashable a) => Hashable (RTree i a)

instance (NFData i, NFData a) => NFData (RTree i a)

instance Foldable (RTree a) where
  foldMap f t = case t of Tip         -> mempty
                          Leaf _ a    -> f a
                          Region _ ts -> foldMap (foldMap f) ts

bounds :: RTree i a -> (i,i)
bounds (Leaf i _)   = (i,i)
bounds (Region b _) = b
bounds Tip          = error "no bounds of empty tree"

maxPageSize :: Int
maxPageSize = 4 -- tuneable page size

size :: RTree i a -> Int
size = sum . fmap (pure 1)

index :: (Ix i, Coord i) => [(i, a)] -> RTree i a
index = go 0
  where
    boundify (i:is) = L.foldl' (\b i -> expand i b) (i,i) is
    go _ [] = Tip
    go dim xs
      | n <= maxPageSize = Region (boundify (fst <$> xs)) (uncurry Leaf <$> NE.fromList xs)
      | otherwise = let ds = dimensions
                        order = view $ runLens (ds !! (dim `mod` length ds))
                        chunk = n `div` (maxPageSize `div` 2)
                        ts = go (dim + 1) <$> chunksOf chunk (L.sortBy (comparing (order.fst)) xs)
                        bs = L.foldl1 expandB (bounds <$> ts)
                     in maybe Tip (Region bs) (NE.nonEmpty ts)
        where n = length xs

chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf n xs = let (as,bs) = L.splitAt n xs in as : chunksOf n bs

insert :: (Ix i, Coord i) => i -> a -> RTree i a -> RTree i a
insert i a = insertT (Leaf i a)

insertT :: (Ix i, Coord i) => RTree i a -> RTree i a -> RTree i a
insertT left Tip  = left
insertT Tip right = right
insertT left right | contains left right = insertT right left
insertT left right = case right of
  (Leaf i _)     -> Region (expand i $ bounds left) (left :| pure right)
  (Region bs ts) -> Region (let (i,j) = bounds left in expand i . expand j $ bounds right)
                           (insertChild left ts)

insertChild :: (Ix i, Coord i) => RTree i a -> NonEmpty (RTree i a) -> NonEmpty (RTree i a)
insertChild t ts | length ts < maxPageSize =
  case NE.partition (`contains` t) ts of
    ([],_)             -> NE.cons t ts
    ((parent:ps), ts') -> insertT t parent :| ps ++ ts'
insertChild t ts = let (best:rest) = L.sortBy (comparing (expansion t)) (NE.toList ts)
                       new = insertT t best
                       (inside,outside) = L.partition (overlaps (bounds new) . bounds) rest
                    in L.foldl' (\parent child -> insertT child parent) new inside :| outside

query :: (Ix i, Coord i) => (i,i) -> RTree i a -> [(i,a)]
query q Tip = []
query q t
  | overlaps q (bounds t) = case t of Leaf i a    -> [(i,a)]
                                      Region _ ts -> NE.toList ts >>= query q
  | otherwise             = []

nearestNeighbour :: (Real b, Ord b, Ix i, Coord i) => (i -> i -> b) -> i -> RTree i a -> Maybe (i,a)
nearestNeighbour _    _   Tip = Nothing
nearestNeighbour dist pnt t   = go init
  where
    q = (pnt,pnt)
    search = (filter ((/= pnt) . fst) .) . query
    dbl :: Double -> Double
    dbl = id
    init = if q `within` bounds t
              then 1
              else ceiling . dbl . realToFrac
                   $ L.minimum (dist pnt <$> corners (bounds t))
    go n = let q' = expandQuery n q in case search q' t of
       []      -> if bounds t `within` q'
                     then Nothing -- the only thing to find is the input
                     else go (2 * n)
       matches -> let d = ceiling . dbl . realToFrac . L.minimum
                          $ fmap (dist pnt . fst) matches
                   in listToMaybe
                      . L.sortBy (comparing (dist pnt . fst))
                      $ search (expandQuery d q) t

nearestNeighbour2 :: (Coord i, Eq i, Real b) => (i -> i -> b) -> i -> RTree i a -> Maybe (i,a)
nearestNeighbour2 dist p = listToMaybe . nearestNeighbourK dist 1 p

-- retrieve k nearest neighbours
nearestNeighbourK :: forall i a b. (Coord i, Eq i, Real b)
                  => (i -> i -> b) -> Int -> i -> RTree i a -> [(i,a)]
nearestNeighbourK dist n pnt
  = go (max 0 n) . enqueue (Heap.empty :: Heap (Entry Double (RTree i a)))
  where
    enqueue q t = case t of
      Tip        -> q
      Leaf i _   | pnt == i -> q
      Leaf i _   -> Heap.insert (Entry (realToFrac $ dist pnt i) t) q
      Region b _ -> Heap.insert (Entry (mindist pnt b) t) q

    go 0      _ = []
    go needed q = maybeToList (Heap.uncons q) >>= \(e,q') -> case Heap.payload e of
            Tip         -> go needed q' -- impossible, but harmless
            Leaf i a    -> (i,a) : go (needed - 1) q'
            Region _ ts -> go needed (L.foldl' enqueue q' ts)

contains :: (Ix i, Coord i) => RTree i a -> RTree i a -> Bool
contains _ Tip                        = True
contains (Region bs _) (Leaf i _)     = Ix.inRange bs i
contains (Region bs _) (Region bs' _) = bs' `within` bs
contains _ _                          = False

expansion :: (Ix i, Coord i) => RTree i a -> RTree i a -> Int
expansion t t' = sizeWith t t' - sizeWith Tip t'

sizeWith :: (Ix i, Coord i) => RTree i a -> RTree i a -> Int
sizeWith Tip  Tip  = 0
sizeWith left Tip  = Ix.rangeSize (bounds left)
sizeWith Tip right = Ix.rangeSize (bounds right)
sizeWith t0 t1 = let (i,j) = bounds t0 in Ix.rangeSize (expand i . expand j $ bounds t1)

expand :: (Ord i, Coord i) => i -> (i,i) -> (i,i)
expand i bs = foldr f bs dimensions
  where f l = let v = i ^. runLens l
               in over (_1.runLens l) (min v) . over (_2.runLens l) (max v)

expandB :: (Ord i, Coord i) => (i,i) -> (i,i) -> (i,i)
expandB (i,j) = expand i . expand j

expandQuery :: (Coord i) => Int -> (i,i) -> (i,i)
expandQuery n q = L.foldl' go q dimensions
  where go (lb,ub) dim = (over (runLens dim) (subtract n) lb, over (runLens dim) (+ n) ub)

scaleQuery :: (Coord i) => Int -> (i,i) -> (i,i)
scaleQuery f q = L.foldl' go q dimensions
  where go (lb,ub) dim = let mx = view (runLens dim) ub
                             mn = view (runLens dim) lb
                             size = 1 + mx - mn
                             mid = (mn + mx) `div` 2
                             diff = (size * f) `div` 2
                          in (set (runLens dim) (mid - diff) lb, set (runLens dim) (mid + diff) ub)

{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes        #-}

module Elves.RTree where

import           Control.Lens              hiding (contains, index)
import           Data.Foldable             (Foldable (foldMap))
import           Data.Ix                   (Ix)
import qualified Data.Ix                   as Ix
import qualified Data.List                 as L
import           Data.List.NonEmpty        (NonEmpty)
import qualified Data.List.NonEmpty        as NE
import           Data.Ord
import           Data.Maybe
import           Test.QuickCheck.Arbitrary (Arbitrary (arbitrary))

import Elves.Coord

data RTree i a = Tip | Leaf i a | Region (i,i) [RTree i a]
             deriving (Show, Eq, Functor)

instance (Arbitrary i, Arbitrary a, Ix i, Coord i) => Arbitrary (RTree i a) where
  arbitrary = fmap index arbitrary

instance Foldable (RTree a) where
  foldMap f t = case t of Tip         -> mempty
                          Leaf _ a    -> f a
                          Region _ ts -> foldMap (foldMap f) ts

bounds :: RTree i a -> (i,i)
bounds (Leaf i _)   = (i,i)
bounds (Region b _) = b
bounds Tip          = error "no bounds of empty tree"

maxPageSize :: Int
maxPageSize = 10 -- tuneable page size

size :: RTree i a -> Int
size = sum . fmap (pure 1)

index :: (Ix i, Coord i) => [(i, a)] -> RTree i a
index = go 0
  where
    boundify (i:is) = L.foldl' (\b i -> expand i b) (i,i) is
    go _ [] = Tip
    go dim xs
      | n <= maxPageSize = Region (boundify (fst <$> xs)) (uncurry Leaf <$> xs)
      | otherwise = let ds = dimensions
                        order = view $ runLens (ds !! (dim `mod` length ds))
                        chunk = n `div` (maxPageSize `div` 2)
                        ts = go (dim + 1) <$> chunksOf chunk (L.sortBy (comparing (order.fst)) xs)
                        bs = L.foldl1 expandB (bounds <$> ts)
                     in Region bs ts
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
  (Leaf i _)     -> Region (expand i $ bounds left) [left,right]
  (Region bs ts) -> Region (let (i,j) = bounds left in expand i . expand j $ bounds right)
                           (insertChild left ts)

insertChild :: (Ix i, Coord i) => RTree i a -> [RTree i a] -> [RTree i a]
insertChild t ts | length ts < maxPageSize =
  case L.partition (`contains` t) ts of
    ([],_)             -> t : ts
    ((parent:ps), ts') -> insertT t parent : ps ++ ts'
insertChild t ts = let (best:rest) = L.sortBy (comparing (expansion t)) ts
                       new = insertT t best
                       (inside,outside) = L.partition (overlaps (bounds new) . bounds) rest
                    in (L.foldl' (\parent child -> insertT child parent) new inside : outside)

query :: (Ix i, Coord i) => (i,i) -> RTree i a -> [(i,a)]
query q Tip = []
query q t
  | overlaps q (bounds t) = case t of Leaf i a    -> [(i,a)]
                                      Region _ ts -> ts >>= query q
  | otherwise             = []

nearestNeighbour :: (Ix i, Coord i) => (i -> i -> Int) -> i -> RTree i a -> Maybe (i,a)
nearestNeighbour _    _   Tip = Nothing
nearestNeighbour dist pnt t = go init
  where
    q = (pnt,pnt)
    search = (filter ((/= pnt) . fst) .) . query
    init = if q `within` bounds t then 1
                                  else L.minimum (dist pnt <$> corners (bounds t))
    go n = let q' = expandQuery n q
            in case search q' t of
                 []      -> if bounds t `within` q'
                               then Nothing -- the only this is the input
                               else go (2 * n)
                 matches -> let d = L.minimum $ fmap (dist pnt . fst) matches
                             in listToMaybe . L.sortBy (comparing (dist pnt . fst))
                                            $ search (expandQuery d q) t

contains :: (Ix i, Coord i) => RTree i a -> RTree i a -> Bool
contains _ Tip = True
contains (Region bs _) (Leaf i _)     = Ix.inRange bs i
contains (Region bs _) (Region bs' _) = bs' `within` bs
contains _ _ = False

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

{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE RankNTypes             #-}
{-# LANGUAGE TypeFamilies           #-}

module Elves.RTree where

import Data.Ord
import qualified Data.List.NonEmpty as NE
import           Data.List.NonEmpty (NonEmpty)
import           Control.Lens hiding (contains)
import qualified Data.List as L
import           Data.Ix      (Ix)
import qualified Data.Ix      as Ix

type Accessor a b = ReifiedLens a a b b

class Coord a where
  dimensions :: [Accessor a Int]

instance Coord Int where
  dimensions = [Lens (lens id (pure id))]

intLens :: (Integral a) => Lens' s a -> Accessor s Int
intLens l = Lens $ lens (fromIntegral . view l) (\s i -> set l (fromIntegral i) s)

instance (Integral a, Integral b) => Coord (a,b) where
  dimensions = [intLens _1, intLens _2]

instance (Integral a, Integral b, Integral c) => Coord (a,b,c) where
  dimensions = [ intLens _1
               , intLens _2
               , intLens _3
               ]

instance (Integral a, Integral b, Integral c, Integral d) => Coord (a,b,c,d) where
  dimensions = [ intLens _1
               , intLens _2
               , intLens _3
               , intLens _4
               ]

data RTree i a = Tip | Leaf i a | Region (i,i) [RTree i a]
             deriving (Show, Eq)

bounds :: RTree i a -> (i,i)
bounds (Leaf i _) = (i,i)
bounds (Region b _) = b
bounds Tip = error "no bounds of empty tree"

maxPageSize :: Int
maxPageSize = 10 -- tuneable page size

index :: (Ix i, Coord i) => [(i, a)] -> RTree i a
index = go 0
  where
    boundify (i:is) = L.foldl' (\b i -> expand i b) (i,i) is
    go _ [] = Tip
    go dim xs | length xs <= maxPageSize = Region (boundify (fst <$> xs)) (uncurry Leaf <$> xs)
    go dim xs = let ds = dimensions
                    order = view $ runLens (ds !! (dim `mod` length ds))
                    (ls,rs) = L.splitAt (length xs `div` 2) (L.sortBy (comparing (order . fst)) xs)
                    l = go (dim + 1) ls
                    r = go (dim + 1) rs
                 in Region (expandB (bounds l) (bounds r)) [l,r]

insert :: (Ix i, Coord i) => RTree i a -> RTree i a -> RTree i a
insert left Tip  = left
insert Tip right = right
insert left right | contains right left = insert right left
insert left right = case right of
  (Leaf i _)     -> Region (expand i $ bounds left) [left,right]
  (Region bs ts) -> Region (let (i,j) = bounds left in expand i . expand j $ bounds right)
                           (insertChild left ts)

insertChild :: (Ix i, Coord i) => RTree i a -> [RTree i a] -> [RTree i a]
insertChild t ts | length ts < maxPageSize = t : ts
insertChild t ts = let (best:rest) = L.sortBy (comparing (sizeWith t)) ts
                       new = insert t best
                       (inside,outside) = L.partition (overlaps (bounds new) . bounds) rest
                    in (L.foldl' (\parent child -> insert child parent) new inside : outside)

query :: (Ix i, Show i, Coord i) => (i,i) -> RTree i a -> [(i,a)]
query q Tip = []
query q t
  | overlaps q (bounds t) = case t of Leaf i a -> [(i,a)]
                                      Region _ ts -> ts >>= query q
  | otherwise             = []

contains :: (Ix i) => RTree i a -> RTree i a -> Bool
contains _ Tip = True
contains (Region bs _) (Leaf i _)         = Ix.inRange bs i
contains (Region bs _) (Region (lb,ub) _) = Ix.inRange bs lb && Ix.inRange bs ub
contains _ _ = False

overlaps :: (Ix i, Coord i) => (i,i) -> (i,i) -> Bool
overlaps a b = any (Ix.inRange a) (corners b) || any (Ix.inRange b) (corners a)

corners :: (Coord i) => (i,i) -> [i]
corners (lb,ub) = L.foldl' f [lb] dimensions
  where
    f cs  l = cs >>= \c -> [c, set (runLens l) (view (runLens l) ub) c]

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

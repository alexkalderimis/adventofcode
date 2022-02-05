{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Elves.Clique (
  Clique,
  clique,
  cliques,
  member,
  numberOfCliques,
  searchGraph,
  leaves
  ) where

import           Data.Tree
import           Data.Set                (Set)
import qualified Data.Set                as S
import           Data.Foldable           (foldl')

-- restricted set - cannot be directly constructed, and we never
-- construct empty cliques (hence no monoid instance).
--
-- Use Foldable methods (length, toList) to deconstruct cliques.
newtype Clique a = Clique { getClique :: Set a }
  deriving (Show, Eq, Foldable)

instance Ord a => Semigroup (Clique a) where
  a <> b = Clique (S.union (getClique a) (getClique b))

member :: (Ord a) => Clique a -> a -> Bool
member (Clique s) x = S.member x s

clique :: (Ord a) => Tree a -> Clique a
clique = Clique . go mempty
  where
    go seen (Node p _)  | S.member p seen = seen
    go seen (Node p fs) = foldl' go (S.insert p seen) fs

cliques :: (Ord a) => Forest a -> [Clique a]
cliques = foldl' go []
  where
    go cs t | any (`member` rootLabel t) cs = cs
    go cs t = clique t : cs

numberOfCliques :: (Ord a) => Forest a -> Int
numberOfCliques = snd . foldl' go (Clique mempty, 0)
  where
    go (seen, n) node | member seen (rootLabel node) = (seen, n)
    go (seen, n) node = (seen <> clique node, n + 1)

searchGraph :: (a -> [a]) -> [a] -> Forest a
searchGraph children = fmap node
  where
    node p = Node p (fmap node (children p))

leaves :: Forest a -> [a]
leaves = (>>= go)
  where go n = case n of
           Node a [] -> [a]
           Node _ ns -> ns >>= go
           

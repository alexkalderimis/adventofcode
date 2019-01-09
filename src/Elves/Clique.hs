module Elves.Clique where

import           Data.Tree
import           Data.Set                (Set)
import qualified Data.Set                as S
import           Data.Foldable           (foldl')

clique :: (Ord a) => Tree a -> Set a
clique = go mempty
  where
    go seen (Node p _)  | S.member p seen = seen
    go seen (Node p fs) = foldl' go (S.insert p seen) fs

numberOfCliques :: (Ord a) => Forest a -> Int
numberOfCliques = snd . foldl' go (mempty, 0)
  where
    go (seen, n) node | S.member (rootLabel node) seen = (seen, n)
    go (seen, n) node = (seen <> clique node, n + 1)

searchGraph :: (a -> [a]) -> [a] -> Forest a
searchGraph children = fmap node
  where
    node p = Node p (fmap node (children p))

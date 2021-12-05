{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Elves.CountMap where

import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import           Data.Maybe
import           Data.Monoid     hiding ((<>))
import           Data.Semigroup

newtype CountMap a = CountMap { countMap :: Map a (Sum Word) }
  deriving (Show, Eq)

instance (Ord a) => Semigroup (CountMap a) where
  (CountMap a) <> (CountMap b) = CountMap (M.unionWith (<>) a b)

instance (Ord a) => Monoid (CountMap a) where
  mempty = CountMap mempty
  mappend = (<>)

size :: CountMap a -> Int
size = M.size . countMap

count :: Ord a => a -> CountMap a -> CountMap a
count a = CountMap . M.insertWith (<>) a 1 . countMap

countOf :: Ord a => a -> CountMap a -> Word
countOf k = getSum . fromMaybe mempty . M.lookup k . countMap

fromList :: Ord a => [a] -> CountMap a
fromList = CountMap . M.fromListWith (<>) . flip zip (repeat 1)

counting :: (Word -> Bool) -> CountMap a -> [a]
counting f = fmap fst . filter (f . getSum . snd) . M.toList . countMap

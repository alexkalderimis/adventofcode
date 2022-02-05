module Elves.CountMap where

import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import           Data.Maybe
import           Data.Monoid     hiding ((<>))
import           Data.Semigroup

type Total = Integer

newtype CountMap a = CountMap { countMap :: Map a (Sum Total) }
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

dec :: Ord a => a -> CountMap a -> CountMap a
dec k (CountMap m) = CountMap (M.alter f k m)
  where
    f mv = mv >>= \n -> case n - 1 of
                          0 -> Nothing
                          m -> Just m

countOf :: Ord a => a -> CountMap a -> Total
countOf k = getSum . fromMaybe mempty . M.lookup k . countMap

{-# INLINE fromList #-}
fromList :: Ord a => [a] -> CountMap a
fromList = CountMap . M.fromListWith (<>) . flip zip (repeat 1)

counting :: (Total -> Bool) -> CountMap a -> [a]
counting f = fmap fst . filter (f . getSum . snd) . M.toList . countMap

{-# INLINE counts #-}
counts :: CountMap a -> [(a, Total)]
counts = fmap (fmap getSum) . M.toList . countMap

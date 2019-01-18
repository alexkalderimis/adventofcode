{-# LANGUAGE DeriveFunctor       #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleInstances   #-}

module Elves.RTree where

import           Prelude                   hiding (null)

import           Control.DeepSeq           (NFData (..))
import           Control.Lens              hiding (contains, index, indexed)
import           Control.Applicative
import           Data.Foldable             (Foldable (foldMap))
import qualified Data.Foldable             as F
import qualified Data.Traversable          (Traversable(traverse))
import           Data.Hashable             (Hashable)
import           Data.Heap                 (Entry (..), Heap)
import qualified Data.Heap                 as Heap
import           Data.Ix                   (Ix)
import qualified Data.Ix                   as Ix
import qualified Data.List                 as L
import           Data.List.NonEmpty        (NonEmpty (..))
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

instance Traversable (RTree i) where
  traverse _ Tip            = pure Tip
  traverse f (Leaf i a)     = Leaf i <$> f a
  traverse f (Region bs ts) = Region bs <$> sequenceA (traverse f <$> ts)

bounds :: RTree i a -> Maybe (i,i)
bounds (Leaf i _)   = Just (i,i)
bounds (Region b _) = Just b
bounds Tip          = Nothing

indexed :: RTree i a -> RTree i (i,a)
indexed Tip = Tip
indexed (Leaf i a) = Leaf i (i,a)
indexed (Region bs ts) = Region bs (indexed <$> ts)

assocs :: RTree i a -> [(i,a)]
assocs = F.toList . indexed

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
                        bs = L.foldl1 expandB (catMaybes (bounds <$> ts))
                     in maybe Tip (Region bs) (NE.nonEmpty ts)
        where n = length xs

chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf n xs = let (as,bs) = L.splitAt n xs in as : chunksOf n bs

insert :: (Ix i, Coord i) => i -> a -> RTree i a -> RTree i a
insert = insertWith pure

insertWith :: (Ix i, Coord i) => (a -> a -> a) -> i -> a -> RTree i a -> RTree i a
insertWith f i a = insertT f (Leaf i a)

delete :: (Ix i, Coord i, Eq i) => i -> RTree i a -> RTree i a
delete i t = case t of
  Tip          -> Tip
  Leaf j _     | i == j -> Tip
  Region bs ts | within (i,i) bs -> untip (Region bs (delete i <$> ts))
  _            -> t
 where
   untip (Region bs ts) = maybe Tip (Region bs) $ NE.nonEmpty $ NE.filter (not . null) ts
   untip x = x

null :: RTree i a -> Bool
null Tip = True
null _   = False

empty :: RTree i a
empty = Tip

insertT :: (Ix i, Coord i) => (a -> a -> a) -> RTree i a -> RTree i a -> RTree i a
insertT f (Leaf i a) (Leaf j b) | i == j = Leaf i (f a b)
insertT f left right = case (bounds left, bounds right) of
  (Nothing, _) -> right
  (_, Nothing) -> left
  (Just lb, Just rb) -> if contains left right then insertT f right left else case right of
    Leaf i _          -> Region (expand i lb) (insertChild f left (pure right))
    Region bs ts      -> Region (expandB lb rb) (insertChild f left ts)

insertChild :: (Ix i, Coord i) => (a -> a -> a) -> RTree i a -> NonEmpty (RTree i a) -> NonEmpty (RTree i a)
insertChild f t ts | length ts < maxPageSize =
  case NE.partition (`contains` t) ts of
    ([],_)             -> NE.cons t ts
    ((parent:ps), ts') -> insertT f t parent :| ps ++ ts'
insertChild f t ts = let (best:rest) = L.sortBy (comparing (expansion t)) (NE.toList ts)
                         new = insertT f t best
                         (inside,outside) = L.partition (overlapping new) rest
                      in L.foldl' (\parent child -> insertT f child parent) new inside :| outside

query :: (Ix i, Coord i) => (i,i) -> RTree i a -> [(i,a)]
query q t
  | Just b <- bounds t
  , overlaps q b = case t of Leaf i a    -> [(i,a)]
                             Region _ ts -> NE.toList ts >>= query q
  | otherwise             = []

nearestNeighbour :: (Coord i, Eq i, Real b) => (i -> i -> b) -> i -> RTree i a -> Maybe (i,a)
nearestNeighbour dist p = listToMaybe . nearestNeighbourK dist 1 p

-- retrieve k nearest neighbours
nearestNeighbourK :: (Coord i, Eq i, Real b)
                  => (i -> i -> b) -> Int -> i -> RTree i a -> [(i,a)]
nearestNeighbourK dist n pnt
  = go (max 0 n) . (enqueue =<< priorityQueue)
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

priorityQueue :: RTree i a -> Heap (Entry Double (RTree i a))
priorityQueue _ = Heap.empty

contains :: (Ix i, Coord i) => RTree i a -> RTree i a -> Bool
contains _ Tip                        = True
contains (Region bs _) (Leaf i _)     = Ix.inRange bs i
contains (Region bs _) (Region bs' _) = bs' `within` bs
contains _ _                          = False

overlapping :: Coord i => RTree i a -> RTree i a -> Bool
overlapping a b = fromMaybe False (liftA2 overlaps (bounds a) (bounds b))

expansion :: (Ix i, Coord i) => RTree i a -> RTree i a -> Int
expansion t t' = sizeWith t t' - extent t

sizeWith :: (Ix i, Coord i) => RTree i a -> RTree i a -> Int
sizeWith t0 t1 = maybe 0 Ix.rangeSize (liftA2 expandB (bounds t0) (bounds t1)
                                       <|> bounds t0
                                       <|> bounds t1
                                      )

extent :: (Ix i) => RTree i a -> Int
extent = maybe 0 Ix.rangeSize . bounds

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

{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleInstances #-}

module Elves.RTree where

import           Prelude                   hiding (lookup, null)

import           Control.Applicative       hiding (empty)
import           Control.Arrow             (first)
import           Control.DeepSeq           (NFData (..))
import           Control.Lens              hiding (contains, index, indexed)
import           Data.Foldable             (Foldable (foldMap))
import qualified Data.Foldable             as F
import           Data.Function             (on)
import           Data.Functor.Classes      (Eq1 (..), Eq2 (..))
import           Data.Hashable             (Hashable)
import           Data.Heap                 (Entry (..), Heap)
import qualified Data.Heap                 as Heap
import           Data.Ix                   (Ix)
import qualified Data.Ix                   as Ix
import qualified Data.List                 as L
import           Data.List.NonEmpty        (NonEmpty (..))
import qualified Data.List.NonEmpty        as NE
import           Data.Maybe
import           Data.Monoid               hiding (First (..), (<>))
import           Data.Ord
import           Data.Semigroup
import qualified Data.Traversable          (Traversable (traverse))
import qualified Data.Tree                 as Tree
import           GHC.Generics              (Generic)

import           Test.QuickCheck.Arbitrary (Arbitrary (arbitrary, shrink),
                                            arbitraryBoundedEnum)
import           Test.QuickCheck.Gen       (suchThat)

import           Elves.Coord
import           Elves.Core

type Bounds i = (i,i)

newtype ValidBounds i = ValidBounds
  { getValidBounds :: (i,i)
  } deriving (Show, Eq)

instance (Arbitrary i, Coord i) => Arbitrary (ValidBounds i) where
  arbitrary = do
    lb <- arbitrary
    ub <- arbitrary `suchThat` allDimensionsAbove lb
    return (ValidBounds (lb,ub))
   where
     allDimensionsAbove lb ub = all (\(Lens d) -> lb^.d <= ub^.d) dimensions

data RTree i a
  = Tip
  | Leaf (Bounds i) a
  | Region (Bounds i) (NonEmpty (RTree i a))
  deriving (Functor, Generic)

instance (Show i, Show a) => Show (RTree i a) where
  showsPrec d t = showParen (d > app_prec) $
                    showString "fromList "
                  . showsPrec (app_prec+1) (assocs t)
         where app_prec = 10

instance (Arbitrary i, Arbitrary a, Ix i, Coord i) => Arbitrary (RTree i a) where
  arbitrary = fmap (fromList . fmap (first getValidBounds)) arbitrary
  shrink t = do
    pairs <- missing1 (assocs t)
    return (fromList pairs)

shrinkList :: Arbitrary a => [a] -> [[a]]
shrinkList [] = [[]]
shrinkList (x:xs) = let xss = shrinkList xs
                     in case shrink x of
                          []     -> xss
                          shrunk -> [x' : xs' | x' <- shrunk, xs' <- xss ]

missing1 :: [a] -> [[a]]
missing1 xs = [pref ++ drop 1 suff | n <- [0 .. length xs - 1]
                                 , let (pref,suff) = L.splitAt n xs
              ]

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

instance (Ix i, Coord i) => Semigroup (RTree i a) where
  (<>) = union

instance (Ix i, Coord i) => Monoid (RTree i a) where
  mempty = empty
  mappend = (<>)

instance Eq2 RTree where
    liftEq2 eqi eqa t1 t2 =
        size t1 == size t2 && liftEq (liftEq2 (eqtup eqi) eqa) (assocs t1) (assocs t2)

eqtup :: (a -> b -> Bool) -> (a,a) -> (b,b) -> Bool
eqtup f a b = (fst a `f` fst b) && (snd a `f` snd b)

instance Eq i => Eq1 (RTree i) where
    liftEq = liftEq2 (==)

instance (Coord i, Ord i, Ord a) => Ord (RTree i a) where
  compare t1 t2 = let sorted = L.sortBy (comparing fst) . assocs
                   in compare (sorted t1) (sorted t2)

-- to avoid exposing internal structure, the EQ instance
-- requires Ord instances of its components
instance (Coord i, Eq i, Eq a) => Eq (RTree i a) where
  a == b = case bounds a <|> bounds b of
             Nothing -> True
             Just (lb,_) -> priorityOrder straightLine lb a == priorityOrder straightLine lb b

bounds :: RTree i a -> Maybe (Bounds i)
bounds (Leaf b _)   = Just b
bounds (Region b _) = Just b
bounds Tip          = Nothing

indexed :: RTree i a -> RTree i (Bounds i, a)
indexed Tip            = Tip
indexed (Leaf i a)     = Leaf i (i,a)
indexed (Region bs ts) = Region bs (indexed <$> ts)

locations :: RTree i a -> [Bounds i]
locations = F.toList . fmap fst . indexed

assocs :: RTree i a -> [((i,i),a)]
assocs = F.toList . indexed

maxPageSize :: Int
maxPageSize = 4 -- tuneable page size

size :: RTree i a -> Int
size = sum . fmap (pure 1)

fromList :: (Ix i, Coord i) => [(Bounds i, a)] -> RTree i a
fromList = fromListWith pure

fromListWith :: (Coord i, Ix i) => (a -> a -> a) -> [(Bounds i, a)] -> RTree i a
fromListWith f = L.foldl' (\t (bs, a) -> unionWith (flip f) (Leaf bs a) t) Tip

insert :: (Ix i, Coord i) => Bounds i -> a -> RTree i a -> RTree i a
insert = insertWith pure

insertPoint :: (Ix i, Coord i) => i -> a -> RTree i a -> RTree i a
insertPoint i = insert (i,i)

insertWith :: (Ix i, Coord i) => (a -> a -> a) -> Bounds i -> a -> RTree i a -> RTree i a
insertWith f i a = unionWith f (Leaf i a)

delete :: (Ix i, Coord i, Eq i) => (Bounds i) -> RTree i a -> RTree i a
delete i t = case t of
  Tip          -> Tip
  Leaf j _     | i == j -> Tip
  Region bs ts | within i bs -> untip (Region bs (delete i <$> ts))
  _            -> t
 where
   untip (Region bs ts) = maybe Tip (Region bs) $ NE.nonEmpty $ NE.filter (not . null) ts
   untip x = x

alter :: (Ix i, Coord i)
      => (Maybe a -> Maybe a) -> Bounds i -> RTree i a -> RTree i a
alter f bs t = case f (lookup bs t) of
  Just x -> insert bs x t
  Nothing -> delete bs t

null :: RTree i a -> Bool
null Tip = True
null _   = False

empty :: RTree i a
empty = Tip

union :: (Ix i, Coord i) => RTree i a -> RTree i a -> RTree i a
union = unionWith pure

-- The meat of the matter - adding two trees together.
unionWith :: (Ix i, Coord i) => (a -> a -> a) -> RTree i a -> RTree i a -> RTree i a
-- the trivial cases: tips are identities of union
unionWith f Tip right = right
unionWith f left Tip  = left
-- when both are leaves, we need to deal with collisions
unionWith f l@(Leaf i a) r@(Leaf j b)
  | i == j    = Leaf i (f a b)
  | otherwise = Region (expandB i j) (sortKids (l :| pure r))
unionWith f l r
  -- when l overlaps r, we need to insert l into r somehow.
  | overlapping l r = compact $ case l of
      Leaf i a    -> let bs' = maybe i (expandB i) (bounds r)
                         ts' = sortKids . insertChild f i a $ subtrees r
                      in Region bs' ts'
      Region _ ts -> F.foldl' (\a t -> unionWith f t a) r ts
  -- safe to use fromJust here, as we have guarded against Tips
  -- in the trivial cases above.
  | otherwise      = let bs' = fromJust (expandB <$> bounds l <*> bounds r)
                         ts' = sortKids (l :| pure r)
                      in Region bs' ts'

sortKids :: (Ord i) => NonEmpty (RTree i a) -> NonEmpty (RTree i a)
sortKids = NE.sortBy (comparing bounds)

-- remove useless intermediary nodes
compact :: RTree i a -> RTree i a
compact (Region _ (t :| [])) = t
compact t                    = t

-- Remove (at-most) one layer of structure, returning the immediately
-- accessible subtrees
--
-- Satisfies: sconcat (subtrees t) === t
subtrees :: RTree i a -> NonEmpty (RTree i a)
subtrees (Region _ ts) = ts
subtrees t             = pure t

-- Decompose a tree into a list of leaves
--
-- Satisfies: mconcat (subtrees t) === t
leaves :: RTree i a -> [RTree i a]
leaves t = case t of
  Region _ ts -> NE.toList ts >>= leaves
  Tip         -> []
  _           -> pure t

insertChild :: (Ix i, Coord i)
            => (a -> a -> a) -> Bounds i -> a
            -> NonEmpty (RTree i a) -> NonEmpty (RTree i a)
insertChild f bs a ts = case (length ts < maxPageSize, none isRegion ts) of
  (True, True)   -> insertInto $ NE.partition colliding ts
  (isSmall, _)   -> maybe (fallback isSmall) insertInto
                    $ foldr (<|>) Nothing [ divide (member bs)
                                          , divide (`contains` t)
                                          , divide (overlapping t)
                                          ]
  where
    -- insert child into best position in the current list. This ensures that
    -- any collisions will always be selected first, followed by overlaps.
    fallback isSmall
      | isSmall && none (overlapping t) ts = NE.cons t ts
      | otherwise = let (best :| rest) = selectChild t ts
                     in unionWith f t best :| rest
    -- given a set of good choices, insert the child into their union
    insertInto (best,rest) = unionWith f t (mconcat best) :| rest
    -- the child
    t = Leaf bs a

    divide f = neitherNull $ NE.partition f ts

    colliding = maybe False (== bs) . bounds
    overlappingReg = liftA2 (&&) (overlapping t) isRegion
    -- choose the child that needs the smallest expansion to contain the child,
    -- in a tie, choose the child that is closest in extent to the child itself.
    -- This is designed to make sure we prefer collisions to containment
    neitherNull (xs,ys) = if L.null xs || L.null ys
                             then Nothing
                             else Just (xs,ys)

selectChild t ts = NE.sortBy (comparing (\x -> (expansion t x, expansion x t))) ts

isRegion :: RTree i a -> Bool
isRegion Region{} = True
isRegion _        = False

data QueryStrategy = Precisely | Within | Overlapping deriving (Show, Eq, Bounded, Enum)

instance Arbitrary QueryStrategy where
  arbitrary = arbitraryBoundedEnum

matches :: (Ix i, Coord i) => QueryStrategy -> Bounds i -> Bounds i -> Bool
matches Precisely   = (==)
matches Within      = within
matches Overlapping = overlaps

query :: (Ix i, Coord i) => QueryStrategy -> (i,i) -> RTree i a -> [(Bounds i,a)]
query strat q t
  | Just b <- bounds t
  , overlaps q b = case t of Leaf i a    -> [ (i,a) | matches strat i q ]
                             Region _ ts -> NE.toList ts >>= query strat q
  | otherwise             = []

lookup :: (Ix i, Coord i) => (i,i) -> RTree i a -> Maybe a
lookup q = fmap snd . listToMaybe . query Precisely q

member :: (Ix i, Coord i) => Bounds i -> RTree i a -> Bool
member q = isJust . lookup q

nearestNeighbour :: (Coord i, Eq i, Real b) => (i -> i -> b) -> i -> RTree i a -> Maybe (Bounds i,a)
nearestNeighbour dist p = listToMaybe . nearestNeighbourK dist 1 p

-- retrieve k nearest neighbours
nearestNeighbourK :: (Coord i, Real b)
                  => (i -> i -> b) -> Int -> i -> RTree i a -> [(Bounds i,a)]
nearestNeighbourK dist n pnt = take n . nearestNeighbours dist pnt

-- returns tree sorted into priority order
-- does not include any objects located precisely at the given point
nearestNeighbours :: (Coord i, Real b)
                  => (i -> i -> b) -> i -> RTree i a -> [(Bounds i,a)]
nearestNeighbours dist pnt = filter ((/= (pnt,pnt)) . fst)
                           . priorityOrder dist pnt

-- returns tree sorted into priority order.
-- Returns all objects in the tree
priorityOrder :: (Coord i, Real b)
              => (i -> i -> b) -> i -> RTree i a -> [(Bounds i,a)]
priorityOrder dist pnt = L.unfoldr go . (enqueue =<< priorityQueue)
  where
    d = realToFrac . dist pnt . closestPoint pnt
    enqueue q t = case fmap d (bounds t) of
      Just d' -> Heap.insert (Entry d' t) q
      _       -> q

    go q = do
      (e,q') <- Heap.uncons q
      case Heap.payload e of
        Tip         -> go q' -- impossible, but harmless
        Leaf i a    -> pure ((i,a),q')
        Region _ ts -> go (L.foldl' enqueue q' ts)

priorityQueue :: RTree i a -> Heap (Entry Double (RTree i a))
priorityQueue _ = Heap.empty

-- is rhs entirely within lhs?
contains :: (Ix i, Coord i) => RTree i a -> RTree i a -> Bool
contains a b = fromMaybe False $ liftA2 within (bounds b) (bounds a)

overlapping :: Coord i => RTree i a -> RTree i a -> Bool
overlapping a b = fromMaybe False $ liftA2 overlaps (bounds a) (bounds b)

-- how much bigger does rhs have to become to encompass lhs?
expansion :: (Ix i, Coord i) => RTree i a -> RTree i a -> Int
expansion t t' | t' `contains` t = 0
expansion t t' = sizeWith t t' - extent t'

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

structure :: (Show i, Show a) => RTree i a -> Tree.Forest String
structure t = case t of
  Tip          -> []
  Leaf i a     -> node (show i <> " => " <> show a) []
  Region bs ts -> node (show bs) (NE.toList ts >>= structure)
 where node = (pure .) . Tree.Node

drawTree :: (Show i, Show a) => RTree i a -> String
drawTree = Tree.drawForest . structure

instance (Eq i, Ix i, Coord i, Show a, Show i, Eq a) => ShowDiff (RTree i a) where
  showDiff a b = case diffTree a b of
                   []    -> Nothing
                   diffs -> Just (show diffs)

diffTree :: (Eq i, Ix i, Coord i, Eq a) => RTree i a -> RTree i a -> [(Bounds i, Where a a)]
diffTree a b = do
  q <- concat . fmap (take 1) . L.group $ L.sort (locations a <> locations b)
  case (lookup q a, lookup q b) of
    (Nothing, Just x)  -> pure (q, InRight x)
    (Just x, Nothing)  -> pure (q, InLeft x)
    (Nothing, Nothing) -> pure (q, Neither) -- impossible
    (Just x, Just y)   -> if x == y then [] else pure (q, Diff x y)


{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE UndecidableInstances #-}

module Elves.RTree (
  Extendable, Bounds, Queryable, QueryStrategy(..), RTree, Neighbour(..),
  singleton, region, point, bounds, bounds', indexed, locations, assocs, elems, keys,
  maxPageSize, size, sizeWith, extent, null,
  fromList, fromPoints, fromPointsWith, fromListWith, 
  alter, alterM, delete, insert, insertPoint, insertWith, empty,
  unions, union, unionWith,
  subtrees, leaves, forest, fromTree, depth, minDepth,
  query, popQuery, overlaps, within,
  member, lookup, priorityOrder, nearestNeighbour, nearestNeighbourK, nearestNeighbours,
  expandQuery, drawTree, diffTree, overlapping
  ) where

import           Prelude                   hiding (lookup, null)

import           Control.Applicative       hiding (empty)
import           Control.Arrow             (first, (&&&))
import           Control.DeepSeq           (NFData (..))
import           Control.Lens              hiding (contains, indexed)
import           Control.Monad             (foldM)
import           Data.Foldable             (Foldable (foldMap))
import qualified Data.Foldable             as F
import           Data.Function             (on)
import           Data.Functor.Classes      (Eq1 (..), Eq2 (..))
import           Data.Functor.Identity     (Identity(..))
import           Data.Hashable             (Hashable)

import           Data.Heap                 (Entry (..), Heap)
import qualified Data.Heap                 as Heap

import qualified Data.List.Extra           as L
import           Data.List.NonEmpty        (NonEmpty (..))
import qualified Data.List.NonEmpty        as NE
import           Data.Maybe
import           Data.Monoid               hiding (First (..), (<>))
import           Data.Ord
import           Data.Semigroup
import qualified Data.Traversable          (Traversable (traverse))
import qualified Data.Tree                 as Tree
import           GHC.Generics              (Generic)
import           GHC.Stack (HasCallStack)

import           Test.QuickCheck.Arbitrary (Arbitrary (arbitrary, shrink),
                                            arbitraryBoundedEnum)
import           Test.QuickCheck.Gen       (chooseInt, suchThat)

import           Elves.Coord (Bounds, Extent, Coord, Dimension, expandB, dimensions, straightLine)
import qualified Elves.Coord as Coord
import           Elves.Core
import           Elves (median)

type Extendable i = (Queryable i, Extent (Dimension i))

type Queryable i =  (Coord i, Num (Dimension i), Eq (Dimension i), Ord (Dimension i))

data Neighbour i a d = Neighbour
  { neighbourLocation :: !(Bounds i)
  , neighbour :: !a
  , neighbourDistance :: !d
  } deriving (Show, Eq)

newtype ArbitraryBounds i = ArbitraryBounds
  { arbitraryBounds :: (i, i)
  } deriving (Show, Eq)

instance (Ord (Dimension i), Coord i, Arbitrary (Dimension i), Arbitrary i)
  => Arbitrary (ArbitraryBounds i) where
  arbitrary = do
    lb <- arbitrary
    ub <- foldM (f lb) Coord.origin dimensions
    
    pure (ArbitraryBounds (lb, ub))
    where
      f lb c d = do x <- arbitrary `suchThat` (>= view (runLens d) lb)
                    pure (set (runLens d) x c)

data RTree i a
  = Tip
  | Leaf   !i !i a
  | Region !i !i (NonEmpty (RTree i a))
  deriving (Functor, Generic)

instance (Show i, Show a) => Show (RTree i a) where
  showsPrec d t = showParen (d > app_prec) $
                    showString "fromList "
                  . showsPrec (app_prec+1) (assocs t)
         where app_prec = 10

instance (Arbitrary (Dimension i), Arbitrary i, Arbitrary a, Extendable i) => Arbitrary (RTree i a) where
  arbitrary = fmap (fromList . fmap (first arbitraryBounds)) arbitrary
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
missing1 xs = [pref <> drop 1 suff | n <- [0 .. length xs - 1]
                                   , let (pref,suff) = L.splitAt n xs
              ]

singleton :: Bounds i -> a -> RTree i a
singleton (lb, ub) = Leaf lb ub

region :: (HasCallStack, Ord (Dimension i), Coord i) => NonEmpty (RTree i a) -> RTree i a
region ts = case NE.filter (not . null) ts of
              [] -> Tip
              [t] -> t
              ts -> let ts' = NE.fromList ts
                        (i,j) = boundingBox ts'
                     in Region i j ts'

instance (Hashable i, Hashable a) => Hashable (RTree i a)

instance (NFData i, NFData a) => NFData (RTree i a)

instance Foldable (RTree a) where
  foldMap f t = case t of Tip         -> mempty
                          Leaf _ _ a    -> f a
                          Region _ _ ts -> foldMap (foldMap f) ts

instance Traversable (RTree i) where
  traverse _ Tip            = pure Tip
  traverse f (Leaf i j a)     = Leaf i j <$> f a
  traverse f (Region i j ts) = Region i j <$> sequenceA (traverse f <$> ts)

instance (Extendable i, Semigroup a) => Semigroup (RTree i a) where
  (<>) = unionWith (<>)

instance (Extendable i, Semigroup a) => Monoid (RTree i a) where
  mempty = empty
  mappend = (<>)
  mconcat ts = case ts of
                 []  -> empty
                 [t] -> t
                 _   -> fromListWith (<>) (ts >>= assocs)

instance (Ord i, Ord a) => Ord (RTree i a) where
  compare t1 t2 = let sorted = L.sortOn fst . assocs
                   in compare (sorted t1) (sorted t2)

-- to avoid exposing internal structure, the EQ instance requires Ord instances of its components
instance (Ord i, Ord a) => Eq (RTree i a) where
  a == b = EQ == compare a b

{-# INLINE point #-}
point :: Coord i => i -> Bounds i
point i = (i,i)

-- inverse of 'point' - whether or not this is in fact a point
-- is not checked.
getPoint :: Coord i => Bounds i -> i
getPoint = fst

{-# INLINE bounds #-}
bounds :: RTree i a -> Maybe (Bounds i)
bounds (Leaf i j _)   = Just (i, j)
bounds (Region i j _) = Just (i, j)
bounds Tip            = Nothing

{-# INLINE bounds' #-}
bounds' :: HasCallStack => RTree i a -> Bounds i
bounds' (Leaf i j _) = (i, j)
bounds' (Region i j _) = (i, j)
bounds' _ = error "Cannot extract bounds of empty tree"

indexed :: RTree i a -> RTree i (Bounds i, a)
indexed Tip            = Tip
indexed (Leaf i j a)   = Leaf i j ((i,j), a)
indexed (Region i j ts) = Region i j (indexed <$> ts)

locations, keys :: RTree i a -> [Bounds i]
locations = F.toList . fmap fst . indexed
keys = locations

assocs :: RTree i a -> [((i,i),a)]
assocs = F.toList . indexed

elems :: RTree i a -> [a]
elems = F.toList

maxPageSize :: forall i a. Coord i => RTree i a -> Int
maxPageSize _ = 2 ^ length dims
  where
    dims :: [Coord.Accessor i (Dimension i)]
    dims = dimensions

size :: RTree i a -> Int
size = sum . fmap (pure 1)

fromList :: (Extendable i) => [(Bounds i, a)] -> RTree i a
fromList = fromListWith pure

fromPoints :: (Extendable i) => [(i, a)] -> RTree i a
fromPoints = fromPointsWith pure

-- indexing points is easier - we just need the bounding boxes, and do not
-- have to worry about overlapping regions.
{-# INLINE fromPointsWith #-}
fromPointsWith :: (Extendable i) => (a -> a -> a) -> [(i, a)] -> RTree i a
fromPointsWith _ [] = empty
fromPointsWith f ps
  | [(k,v)] <- ps'
  = singleton (k, k) v

  | t <- bulkLoadPoints ps'
  , longerThan (maxPageSize t) ps'
  = t

  | otherwise = region (NE.fromList [ singleton (point p) x | (p, x) <- ps' ])
  where 
    ps' = ungroup f ps

bulkLoadPoints :: (Extendable i) => [(i, a)] -> RTree i a
bulkLoadPoints ps
  = let seeds'  = (, []) <$> seeds [ (fst p, fst p) | p <- ps ]
        buckets = F.foldl' place seeds' ps
     in untip $ region ( fromPoints . snd <$> buckets )
  where
    -- find the first place we can put the point.
    place ((r, ps) :| tl) p = if Coord.containsP (fst p) r
                                then (r, p : ps) :| tl
                                else let tl' = foldMap (NE.toList . (`place` p)) (NE.nonEmpty tl)
                                      in (r, ps) :| tl'

fromListWith :: forall i a. (Extendable i)
             => (a -> a -> a) -> [(Bounds i, a)] -> RTree i a
fromListWith f kvs =
  let tip = Tip :: RTree i a
      pageSize = maxPageSize tip
   in case ungroup f kvs of
        []  -> tip
        [a] -> singleton' a
        xs  -> if longerThan pageSize xs
               then bulkLoad xs
               else region (singleton' <$> NE.fromList xs)
  where
    singleton' (k,v) = singleton k v

-- group by keys, and then eliminate the groups with the monoidal function
{-# INLINE ungroup #-}
ungroup :: Ord k => (a -> a -> a) -> [(k, a)] -> [(k, a)]
ungroup f kvs = [ (head ks, L.foldl1 f vs) | grp <- L.groupOn fst (L.sortOn fst kvs)
                                           , let (ks, vs) = unzip grp
                                           ]

-- general indexing is complicated. We prefer the smaller groups if we can
-- (fitting to the closest corner). Since keys can extend over a range of the area,
-- we need to repack the groups so that none is too large/small.
--
-- For instance, imagine the 1D ranges:
--
--  0-----------------10
--      2-------------10
--           5--------10
--               7----10
--                  9-10
--
-- These are all close to the `10` "corner", even though the bounds
-- of these ranges overall is `0..10`. We may need to move some of these
-- ranges to the `0` corner, even though they have more overlap with the `10`
-- corner.
--
-- Also, due to integer division, we would have a `bestSize` here of 2, but
-- one group will need to be given 3 ranges.
bulkLoad :: Extendable i => [(Bounds i, a)] -> RTree i a
bulkLoad kvs
  = let buckets = take (length cnrs) $ bucketOn (bestCnr cnrs) kvs <> repeat []
        (trimmed, overflows) = unzip $ fmap (L.splitAt bestSize) buckets
        filled = allocateOverhang $ L.foldl'
                     (\(unclaimed, fixed) b ->
                        let (extra, unclaimed') = L.splitAt (bestSize - length b) unclaimed
                        in (unclaimed', (b <> extra) : fixed))
                     (concat overflows, [])
                     trimmed
        ts = fromList <$> filled
     in if sum (length <$> filled) /= length kvs
           then error ("expected " <> show (length kvs) <> ", got " <> show (sum $ fmap length filled))
           else untip $ region (NE.fromList ts)
  where
    box = L.foldl1 expandB (fst <$> kvs)
    cnrs = Coord.corners box

    bestSize = length kvs `div` length cnrs

    bestCnr cs (k, a) = L.minimumOn (measure k) cs
    measure k cnr = uncurry Coord.size (expandB k (cnr, cnr))

    bucketOn f xs = fmap (fmap snd) . L.groupOn fst $ L.sortOn fst [ (f x, x) | x <- xs ]

    -- the use of `div` in calculating bestSize means we may have an overhang.
    allocateOverhang (overhang, grps) = let (some, rest) = L.splitAt (length overhang) grps
                                         in zipWith (:) overhang some <> rest

-- a set of bounds, equally dividing up the total space
{-# INLINE seeds #-}
seeds :: forall i a. Extendable i => [Bounds i] -> NonEmpty (Bounds i)
seeds bs
  = let dims = dimensions
        box = L.foldl1 expandB bs
        bss = L.foldl' splitBounds [box] dims
     in NE.fromList bss
  where
    n = length bs
    dims :: [Coord.Accessor i (Dimension i)]
    dims = dimensions
    splitBounds unsplit d = let read = view (runLens d)
                                write = set (runLens d)
                                med = median [ Coord.midpoint (read p) (read p') | (p, p') <- bs ]
                             in unsplit >>= \(lb, ub) -> let mp = Coord.midpoint (read lb) (read ub)
                                                             x = fromMaybe mp med
                                                             ((_,a), (b, _)) = Coord.divideAt (read lb, read ub) x
                                                          in [(lb, write a ub), (write b lb, ub)]

insert :: (Extendable i) => Bounds i -> a -> RTree i a -> RTree i a
insert = insertWith pure

insertPoint :: (Extendable i) => i -> a -> RTree i a -> RTree i a
insertPoint i = insert (i,i)

insertWith :: (Extendable i) => (a -> a -> a) -> Bounds i -> a -> RTree i a -> RTree i a
insertWith f k a = alter f' k
  where f' = maybe (pure a) (pure . f a)

delete :: (Extendable i) => Bounds i -> RTree i a -> RTree i a
delete = alter (pure Nothing)

-- edit a value in place, with the option to insert/delete
alter :: (Extendable i)
      => (Maybe a -> Maybe a) -> Bounds i -> RTree i a -> RTree i a
alter f k = runIdentity . alterM (Identity . f) k

-- edit a value in place, with the option to insert/delete, using a monadic context
alterM :: (Extendable i, Monad m)
       => (Maybe a -> m (Maybe a)) -> Bounds i -> RTree i a -> m (RTree i a)
alterM f k t
  | Tip <- t
  = whenAbsent empty Nothing (singleton k)

  | Leaf lb ub a <- t
  = if k == (lb, ub)
       then whenAbsent empty (Just a) (singleton k)
       else whenAbsent t Nothing $ \v -> region (singleton k v :| [t])

  | Region lb ub ts <- t
  , (xs, ys)        <- NE.partition (includes k) ts
  , (here, there)   <- L.partition (member k) xs
  = case (here, there <> ys) of
      ([], _)    -> whenAbsent t Nothing $ \v -> region (insertChild pure k v ts)
      ([x], ts') -> do x' <- untip <$> alterM f k x
                       pure $ region (x' :| ts')
      _ -> error "Illegal tree: key must be member of unique path"

  where
    whenAbsent t ma k = do maybe t k <$> f ma
    maybeTree = maybe Tip (singleton k)
    maybeInsert = maybe t (\x -> insert k x t)

-- remove all tips from within the top level of the tree, and resize if needed.
untip :: (Ord (Dimension i), Coord i) => RTree i a -> RTree i a
untip (Region _ _ ts) | any null ts = region ts
untip t = t

deepuntip :: (Ord (Dimension i), Coord i) => RTree i a -> RTree i a
deepuntip (Region _ _ ts) = region (fmap deepuntip ts)
deepuntip t = t


null :: RTree i a -> Bool
null Tip = True
null _   = False

empty :: RTree i a
empty = Tip

union :: (Extendable i)
      => RTree i a -> RTree i a -> RTree i a
union = unionWith pure

unions :: (Extendable i, Foldable f) => f (RTree i a) -> RTree i a
unions = fromList . F.foldMap assocs

-- The meat of the matter - adding two trees together.
unionWith :: (HasCallStack, Extendable i)
          => (a -> a -> a) -> RTree i a -> RTree i a -> RTree i a
-- the trivial cases: tips are identities of union
unionWith f Tip right = right
unionWith f left Tip  = left
unionWith f l@(Leaf i j a) r = insertWith f (i,j) a r
unionWith f l r@(Leaf i j a) = insertWith (flip f) (i,j) a l
unionWith f l r = L.foldl' (flip (unionWith f)) r (leaves l)

sortKids :: (Ord i) => NonEmpty (RTree i a) -> NonEmpty (RTree i a)
sortKids = NE.sortBy (comparing bounds)

-- remove useless intermediary nodes
compact :: RTree i a -> RTree i a
compact (Region _ _ (t :| [])) = t
compact t                      = t

-- Remove (at-most) one layer of structure, returning the immediately
-- accessible subtrees
--
-- Satisfies: sconcat (subtrees t) === t
subtrees :: RTree i a -> NonEmpty (RTree i a)
subtrees (Region _ _ ts) = ts
subtrees t               = pure t

-- Decompose a tree into a list of leaves
--
-- Satisfies: mconcat (subtrees t) === t
leaves :: RTree i a -> [RTree i a]
leaves t = case t of
  Region _ _ ts -> NE.toList ts >>= leaves
  Tip           -> []
  _             -> pure t

-- obtain the full structure of the tree.
forest :: RTree i a -> Tree.Forest (i, i, Maybe a)
forest Tip = []
forest (Leaf i j a)    = pure $ Tree.Node (i, j, pure a)  []
forest (Region i j ts) = pure $ Tree.Node (i, j, Nothing) (NE.toList ts >>= forest)

-- inverse of forest, exposed for testing
fromTree :: Tree.Tree (i, i, Maybe a) -> RTree i a
fromTree (Tree.Node (i, j, Just x) []) = Leaf j j x
fromTree (Tree.Node (i, j, Nothing) ns) = Region i j (NE.fromList $ fmap fromTree ns)
fromTree _ = error "Cannot restore tree"

insertChild :: (Extendable i)
            => (a -> a -> a) -> Bounds i -> a
            -> NonEmpty (RTree i a) -> NonEmpty (RTree i a)
insertChild f k a ts 
  | (collisions, rest) <- NE.partition (collides k) ts
  , not (L.null collisions)
  , not (L.null rest)
  = insertWith f k a (unions collisions) :| rest

  | length ts < maxPageSize t
  = pure t <> ts

  | (best :| rest) <- selectChild t ts -- (joinOverlapping ts)
  = insertWith f k a best :| rest

  where
    -- the child
    t = singleton k a

joinOverlapping :: Extendable i => NonEmpty (RTree i a) -> NonEmpty (RTree i a)
joinOverlapping (t :| ts)
  = case L.partition (overlapping t) ts of
      (xs, ys) -> unions xs :| ys

-- does this tree collide with these bounds? (i.e. are the bounds identical)
collides bs = (Just bs ==) . bounds

-- choose the child that needs the smallest expansion to contain the child,
-- in a tie, choose the child that is closest in extent to the child itself.
-- This is designed to make sure we prefer collisions to containment
selectChild t = NE.sortBy . comparing $ \x -> (expansion t x, size t)

isRegion :: RTree i a -> Bool
isRegion Region{} = True
isRegion _        = False

data QueryStrategy = Precisely | Within | Overlapping deriving (Show, Eq, Bounded, Enum)

instance Arbitrary QueryStrategy where
  arbitrary = arbitraryBoundedEnum

matches :: (Queryable i) => QueryStrategy -> Bounds i -> Bounds i -> Bool
matches Precisely   = (==)
matches Within      = Coord.contains
matches Overlapping = Coord.overlaps

query :: (Queryable i) => QueryStrategy -> (i,i) -> RTree i a -> [(Bounds i,a)]
query strat q t
  | Region i j ts <- t
  , matches Overlapping (i, j) q
  = NE.toList ts >>= query strat q

  | Leaf i j a <- t
  , matches strat (i, j) q
  = pure ( (i, j), a )

  | otherwise = []

-- sugar for common use cases
overlaps :: (Queryable i) => (i,i) -> RTree i a -> [(Bounds i,a)]
overlaps = query Overlapping

within :: (Queryable i) => (i,i) -> RTree i a -> [(Bounds i,a)]
within = query Within

-- find and remove matching values from the tree
popQuery :: (Queryable i) => QueryStrategy -> (i,i) -> RTree i a -> ([(Bounds i, a)], RTree i a)
popQuery strat q t
  | Region i j ts <- t
  , matches Overlapping (i, j) q
  , (found, ts') <- unzip . fmap (popQuery strat q) $ NE.toList ts
  = (concat found, untip (region $ NE.fromList ts'))

  | Leaf i j a <- t
  , matches strat (i, j) q
  = ( pure ((i, j), a), empty )

  | otherwise = ([], t)

lookup :: (Queryable i) => Bounds i -> RTree i a -> Maybe a
lookup q = fmap snd . listToMaybe . query Precisely q

member :: (Queryable i) => Bounds i -> RTree i a -> Bool
member q = isJust . lookup q

nearestNeighbour :: (Queryable i, Eq i, Real b) => (i -> i -> b) -> i -> RTree i a -> Maybe (Bounds i,a)
nearestNeighbour dist p = listToMaybe . nearestNeighbourK dist 1 p

-- retrieve k nearest neighbours
nearestNeighbourK :: (Queryable i, Real b)
                  => (i -> i -> b) -> Int -> i -> RTree i a -> [(Bounds i,a)]
nearestNeighbourK dist n pnt = take n . nearestNeighbours dist pnt

-- returns items in the tree sorted into priority order, defined by distance
-- to a given point.
--
-- Does not include any objects located precisely at the given point
-- If you need to include these, consider using 'lookup' to check for their
-- presence.
nearestNeighbours :: (Queryable i, Real b)
                  => (i -> i -> b) -> i -> RTree i a -> [(Bounds i,a)]
nearestNeighbours dist pnt = fmap (neighbourLocation &&& neighbour)
                           . filter ((/= (pnt,pnt)) . neighbourLocation)
                           . priorityOrder dist pnt

-- returns tree sorted into priority order.
-- Returns all objects in the tree, along with the output of the distance function.
priorityOrder :: (Queryable i, Ord b)
              => (i -> i -> b) -> i -> RTree i a -> [Neighbour i a b]
priorityOrder dist pnt = L.unfoldr go . enqueue Heap.empty
  where
    d = (dist pnt . Coord.closestPoint pnt) &&& id

    enqueue q t = case fmap d (bounds t) of
      Just d' -> Heap.insert (Entry d' t) q
      _       -> q -- ensures we never enqueue tips

    go q = do
      (e,q') <- Heap.uncons q
      case Heap.payload e of
        Tip           -> go q' -- impossible, but harmless
        Leaf _ _ a    -> let (b, bs) = Heap.priority e
                          in Just (Neighbour bs a b, q')
        Region _ _ ts -> go (L.foldl' enqueue q' ts)

-- is rhs entirely within lhs?
contains :: (Coord i, Ord (Dimension i), Num (Dimension i))
         => RTree i a -> RTree i a -> Bool
contains a b = fromMaybe False $ liftA2 Coord.contains (bounds b) (bounds a)

includes :: (Coord k, Ord (Dimension k)) => Bounds k -> RTree k a -> Bool
includes bs = maybe False (Coord.contains bs) . bounds

overlapping :: (Coord i, Num (Dimension i), Ord (Dimension i))
            => RTree i a -> RTree i a -> Bool
overlapping a b = fromMaybe False $ liftA2 Coord.overlaps (bounds a) (bounds b)

-- how much bigger does rhs hand need to be to encompass lhs?
expansion :: (Extendable i) => RTree i a -> RTree i a -> Dimension i
expansion t t' | t' `contains` t = 0
expansion t t' = sizeWith t t' - extent t'

sizeWith :: (Extendable i)
         => RTree i a -> RTree i a -> Dimension i
sizeWith t0 t1 = maybe 0 (uncurry Coord.size) $
     liftA2 expandB (bounds t0) (bounds t1)
      <|> bounds t0
      <|> bounds t1

-- the space covered by this tree - including empty space.
extent :: (Coord i, Extent (Dimension i)) => RTree i a -> Dimension i
extent = maybe 0 (uncurry Coord.size) . bounds

boundingBox :: (HasCallStack, Ord (Dimension i), Coord i) => NonEmpty (RTree i a) -> Bounds i
boundingBox ts = let (b :| bs) = bounds' <$> ts in L.foldl' expandB b bs

expandQuery :: (Ord (Dimension i), Num (Dimension i), Coord i)
            => Int -> (i,i) -> (i,i)
expandQuery n q = L.foldl' go q dimensions
  where n' = fromIntegral n
        go (lb,ub) dim = (over (runLens dim) (subtract n') lb, over (runLens dim) (+ n') ub)

scaleQuery :: (Coord a, Extent (Dimension a)) => Dimension a -> Bounds a -> Bounds a
scaleQuery f q = L.foldl' go q dimensions
  where go (lb,ub) dim = let (a,b) = Coord.scaleBounds f (view (runLens dim) lb, view (runLens dim) ub)
                          in (set (runLens dim) a lb, set (runLens dim) b ub)

structure :: (Show i, Show a) => RTree i a -> Tree.Forest String
structure = fmap (fmap f) . forest
  where
    f (i, j, mv) = mconcat [show i, "..", show j, maybe "" ((" => " <>) . show) mv]

drawTree :: (Show i, Show a) => RTree i a -> String
drawTree = Tree.drawForest . structure

instance (Queryable i, Show i, Eq a, Show a) => ShowDiff (RTree i a) where
  showDiff a b = case diffTree a b of
                   []    -> Nothing
                   diffs -> Just (show diffs)

diffTree :: (Queryable i, Eq a) => RTree i a -> RTree i a -> [(Bounds i, Where a a)]
diffTree a b = do
  q <- concatMap (take 1) . L.group $ L.sort (locations a <> locations b)
  case (lookup q a, lookup q b) of
    (Nothing, Just x)  -> pure (q, InRight x)
    (Just x, Nothing)  -> pure (q, InLeft x)
    (Nothing, Nothing) -> pure (q, Neither) -- impossible
    (Just x, Just y)   -> if x == y then [] else pure (q, Diff x y)

longerThan :: Int -> [a] -> Bool
longerThan n xs = case xs of
  [] -> False
  (_ : tl) -> n <= 0 || longerThan (n - 1) tl

depth :: RTree i a -> Int
depth (Region _ _ ts) = 1 + F.maximum (depth <$> ts)
depth t = 0

minDepth :: RTree i a -> Int
minDepth (Region _ _ ts) = 1 + F.minimum (minDepth <$> ts)
minDepth t = 0

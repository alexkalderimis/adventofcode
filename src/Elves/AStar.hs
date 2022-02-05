{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances
           , AllowAmbiguousTypes
           , ScopedTypeVariables
           , ViewPatterns
           , FunctionalDependencies
           #-}

module Elves.AStar (
    aStar, aStarOrd, aStarM,
    CartesianCoordinate(..), euclideanDistance
    ) where

import           Prelude hiding (lookup)

import           Control.Monad (foldM)
import           Control.Arrow (first)
import           Data.Foldable (toList, foldl')

-- priority queues
import           Data.Heap                 (Entry (..), Heap)
import qualified Data.Heap                 as Heap

-- hashable types
import           Data.Hashable (Hashable(..))
import qualified Data.HashSet as HS
import           Data.HashSet (HashSet)
import qualified Data.HashMap.Strict as HM
import           Data.HashMap.Strict (HashMap)

-- ord types
import qualified Data.Map.Strict as M
import qualified Data.Set as S

-- we need generic maps and sets, supporting a small subset
-- of their usual interfaces:

class GMap map k where
    insert :: k -> v -> map k v -> map k v
    get :: map k v -> k -> v
    lookup :: map k v -> k -> Maybe v
    fromList :: [(k, v)] -> map k v

class (Foldable s, Monoid (s a)) => GSet s a where
    add :: a -> s a -> s a
    member :: a -> s a -> Bool

-- for Hashable keys, we can use HashMap, HashPSQ and HashSet

instance (Hashable k, Eq k) => GMap HashMap k where
    insert = HM.insert
    get = (HM.!)
    lookup = flip HM.lookup
    fromList = HM.fromList

instance (Hashable a, Eq a) => GSet HashSet a where
    add = HS.insert
    member = HS.member

-- for Ord keys, we use Data.Map.Strict.Map, OrdPSQ and Data.Set.Set

instance (Eq k, Ord k) => GMap M.Map k where
    insert = M.insert
    get = (M.!)
    lookup = flip M.lookup
    fromList = M.fromList

instance (Ord a) => GSet S.Set a where
    add = S.insert
    member = S.member

data AStar set map cost a = AStar
    { visited  :: !(set a) 
    , waiting  :: !(Heap (Entry cost a))
    , score    :: !(map a cost)
    , memoHeur :: !(map a cost) -- in monadic contexts, heuristics may be expensive to calculate, so store them.
    , cameFrom :: !(map a a)
    , end      :: !(Maybe a)
    }

aStarInit :: (Num c, Ord c, GSet s a, GMap m a)
          => a -> AStar s m c a
aStarInit start = AStar { visited  = mempty
                        , waiting  = Heap.singleton (Entry 0 start)
                        , score    = fromList [(start, 0)]
                        , memoHeur = fromList []
                        , cameFrom = fromList []
                        , end      = Nothing
                        }

runAStar :: (Foldable f, Hashable a, Ord a, Ord c, Num c)
         => (a -> f a) -- adjacencies in graph
         -> (a -> a -> c)    -- distance function
         -> (a -> c)         -- heuristic distance to goal
         -> (a -> Bool)      -- goal
         -> a                -- starting vertex
         -> AStar HashSet HashMap c a -- final state

runAStar graph dist heur goal = runAStarG graph dist heur goal . aStarInit

{-# INLINE runAStarG #-}
runAStarG :: (Foldable f, Num c, Ord a, Ord c, GSet s a, GMap m a)
          => (a -> f a)    -- adjacencies in graph
          -> (a -> a -> c) -- distance function
          -> (a -> c)      -- heuristic distance to goal. This function is expected to be cheap! If not,
                           --  then consider pre-computing it and returning a map-lookup function.
          -> (a -> Bool)   -- goal
          -> AStar s m c a -- initial state
          -> AStar s m c a -- final state

runAStarG successors dist heur goal = go
  where go s
          = case minView (waiting s) of
              Nothing                            -> s
              Just (x, _) | goal x               -> s { end = Just x }
              Just (x, w) | member x (visited s) -> go s { waiting = w }
              Just (x, w) -> go $ foldl' (expand x)
                                         (s { waiting = w
                                            , visited = add x (visited s)
                                            })
                                         (successors x)
        expand x s y
          = let vxy = (score s `get` x) + dist x y
             in case score s `lookup` y of
               Nothing            -> link x y vxy s
               Just vy | vxy < vy -> link x y vxy s
               _                  -> s

        link x y v s
          = let h = v + heur y
            in s { cameFrom = insert  y x (cameFrom s)
                 , score    = insert  y v (score s)
                 , waiting  = enqueue y h (waiting s)
                 }

{-# INLINE minView #-}
minView :: Heap (Entry c a) -> Maybe (a, Heap (Entry c a))
minView = fmap (first Heap.payload) . Heap.viewMin

{-# INLINE enqueue #-}
enqueue :: Ord c => a -> c -> Heap (Entry c a) -> Heap (Entry c a)
enqueue item cost = Heap.insert (Entry cost item)

-- | This function computes an optimal (minimal distance) path through a graph in a best-first fashion,
-- starting from a given starting point.
aStar :: (Foldable f, Hashable a, Ord a, Ord c, Num c) =>
         (a -> f a)     -- ^ The graph we are searching through, given as a function from vertices
                          -- to their neighbours.
         -> (a -> a -> c) -- ^ Distance function between neighbouring vertices of the graph. This will
                          -- never be applied to vertices that are not neighbours, so may be undefined
                          -- on pairs that are not neighbours in the graph.
         -> (a -> c)      -- ^ Heuristic distance to the (nearest) goal. This should never overestimate the
                          -- distance, or else the path found may not be minimal.
         -> (a -> Bool)   -- ^ The goal, specified as a boolean predicate on vertices.
         -> a             -- ^ The vertex to start searching from.
         -> Maybe [a]     -- ^ An optimal path, if any path exists. This excludes the starting vertex.
aStar graph dist heur goal start
  = unwindState start $ runAStar graph dist heur goal start

runAStarM :: forall a c m f. (Monad m, Foldable f, Hashable a, Ord a, Ord c, Num c) =>
          (a -> m (f a))   -- adjacencies in graph
          -> (a -> a -> m c) -- distance function
          -> (a -> m c)      -- heuristic distance to goal
          -> (a -> m Bool)   -- goal
          -> a               -- starting vertex
          -> m (AStar HashSet HashMap c a)   -- final state

runAStarM graph dist heur goal start = go (aStarInit start :: AStar HashSet HM.HashMap c a)
  where go s
          = case minView (waiting s) of
              Nothing     -> pure s
              Just (x, w) | member x (visited s) -> go s { waiting = w }
              Just (x, w) ->
                do g <- goal x
                   if g then return (s { end = Just x })
                        else do ns <- graph x
                                u <- foldM (expand x)
                                           (s { waiting = w,
                                                visited = add x (visited s)})
                                           ns
                                go u
        expand x s y
          = do d <- dist x y
               let v = (score s `get` x) + d
               case score s `lookup` y of
                 Nothing -> do h <- heur y
                               let s' = s { memoHeur = insert y h (memoHeur s) }
                               pure (link x y v s')
                 Just vy | v < vy -> pure (link x y v s)
                 _ -> pure s

        link x y v s
           = s { cameFrom = insert y x (cameFrom s),
                 score    = insert y v (score s),
                 waiting  = enqueue y (v + (memoHeur s `get` y)) (waiting s) }

-- | This function computes an optimal (minimal distance) path through a graph in a best-first fashion,
-- starting from a given starting point.
aStarM :: (Foldable f, Monad m, Hashable a, Ord a, Ord c, Num c) =>
         (a -> m (f a))     -- ^ The graph we are searching through, given as a function from vertices
                            -- to their neighbours.
         -> (a -> a -> m c) -- ^ Distance function between neighbouring vertices of the graph. This will
                            -- never be applied to vertices that are not neighbours, so may be undefined
                            -- on pairs that are not neighbours in the graph.
                            -- always called as `dist from to`, allowing bi-directional edges with different costs
         -> (a -> m c)      -- ^ Heuristic distance to the (nearest) goal. This should never overestimate the
                            -- distance, or else the path found may not be minimal.
         -> (a -> m Bool)   -- ^ The goal, specified as a boolean predicate on vertices.
         -> m a             -- ^ The vertex to start searching from.
         -> m (Maybe [a])   -- ^ An optimal path, if any path exists. This excludes the starting vertex.
aStarM graph dist heur goal start
    = do sv <- start
         s <- runAStarM graph dist heur goal sv
         return $ unwindState sv s

-- | This function computes an optimal (minimal distance) path through a graph in a best-first fashion,
-- starting from a given starting point.
-- This function accepts non-Hashable items
aStarOrd :: forall f a c. (Foldable f, Ord a, Ord c, Num c) =>
         (a -> f a)     -- ^ The graph we are searching through, given as a function from vertices
                          -- to their neighbours.
         -> (a -> a -> c) -- ^ Distance function between neighbouring vertices of the graph. This will
                          -- never be applied to vertices that are not neighbours, so may be undefined
                          -- on pairs that are not neighbours in the graph.
                            -- always called as `dist from to`, allowing bi-directional edges with different costs
         -> (a -> c)      -- ^ Heuristic distance to the (nearest) goal. This should never overestimate the
                          -- distance, or else the path found may not be minimal.
         -> (a -> Bool)   -- ^ The goal, specified as a boolean predicate on vertices.
         -> a             -- ^ The vertex to start searching from.
         -> Maybe [a]     -- ^ An optimal path, if any path exists. This excludes the starting vertex.
aStarOrd graph dist heur goal start
  = unwindState start
  . runAStarG graph dist heur goal
  $ (aStarInit start :: AStar S.Set M.Map c a)

unwindState :: (GMap m a, Eq a) => a -> AStar s m c a -> Maybe [a]
unwindState start s = unwind (cameFrom s) start <$> end s

unwind :: (GMap m a, Eq a) => m a a -> a -> a -> [a]
unwind history start = reverse
                     . takeWhile (/= start)
                     . iterate (history `get`)

-- General interface for cartesian co-ordinates.
-- Instances are provided for 2 and 3 dimensional tuples
class (Real n) => CartesianCoordinate x n | x -> n where
    points :: x -> [n]

instance (Real n) => CartesianCoordinate (n, n) n where
    {-# INLINE points #-}
    points (x, y) = [x, y]

instance (Real n) => CartesianCoordinate (n, n, n) n where
    {-# INLINE points #-}
    points (x, y, z) = [x, y, z]

instance (Real n) => CartesianCoordinate (n, n, n, n) n where
    {-# INLINE points #-}
    points (x, y, z, w) = [x, y, z, w]
    
-- general purpose function for computing a straight-line distance
-- between two points on a Cartesian plane of an arbitrary number of dimensions
{-# INLINE euclideanDistance #-}
euclideanDistance :: (CartesianCoordinate c n, Real n, Floating b) => c -> c -> b
euclideanDistance (points -> ps) (points -> qs)
  = sqrt . sum . fmap ((^ (2 :: Int)) . realToFrac . abs) $ zipWith subtract ps qs

{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
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
import           Data.Foldable (toList, foldl')

-- hashable types
import           Data.Hashable (Hashable(..))
import qualified Data.HashSet as HS
import           Data.HashSet (HashSet)
import qualified Data.HashMap.Strict as HM
import           Data.HashMap.Strict (HashMap)
import qualified Data.HashPSQ as HPQ
import           Data.HashPSQ (HashPSQ)

-- ord types
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import qualified Data.OrdPSQ as OPQ
import           Data.OrdPSQ (OrdPSQ)

-- we need generic maps, queues and sets, supporting a small subset
-- of their usual interfaces:

class GMap map k where
    insert :: k -> v -> map k v -> map k v
    get :: map k v -> k -> v
    fromList :: [(k, v)] -> map k v

class GQueue q k p where
    singleton :: k -> p -> q k p ()
    minView :: q k p () -> Maybe (k, p, (), q k p ())
    lookup :: k -> q k p v -> Maybe (p, v)
    enqueue :: k -> p -> q k p () -> q k p ()

class (Foldable s, Monoid (s a)) => GSet s a where
    add :: a -> s a -> s a
    difference :: s a -> s a -> s a

-- for Hashable keys, we can use HashMap, HashPSQ and HashSet

instance (Hashable k, Eq k) => GMap HashMap k where
    insert = HM.insert
    get = (HM.!)
    fromList = HM.fromList

instance (Hashable k, Ord k, Ord p) => GQueue HashPSQ k p where
    singleton k p = HPQ.singleton k p ()
    minView = HPQ.minView
    lookup = HPQ.lookup
    enqueue k p = HPQ.insert k p ()

instance (Hashable a, Eq a) => GSet HashSet a where
    add = HS.insert
    difference = HS.difference

-- for Ord keys, we use Data.Map.Strict.Map, OrdPSQ and Data.Set.Set

instance (Eq k, Ord k) => GMap M.Map k where
    insert = M.insert
    get = (M.!)
    fromList = M.fromList

instance (Ord k, Ord p) => GQueue OrdPSQ k p where
    singleton k p = OPQ.singleton k p ()
    minView = OPQ.minView
    lookup = OPQ.lookup
    enqueue k p = OPQ.insert k p ()

instance (Ord a) => GSet S.Set a where
    add = S.insert
    difference = S.difference

data AStar s q m a c = AStar
    { visited  :: !(s a) 
    , waiting  :: !(q a c ())
    , score    :: !(m a c)
    , memoHeur :: !(m a c)
    , cameFrom :: !(m a a)
    , end      :: !(Maybe a)
    }

aStarInit :: (Num c, GSet s a, GMap m a , GQueue q a c)
          => a -> AStar s q m a c
aStarInit start = AStar { visited  = mempty
                        , waiting  = singleton start 0
                        , score    = fromList [(start, 0)]
                        , memoHeur = fromList []
                        , cameFrom = fromList []
                        , end      = Nothing
                        }

runAStar :: (Hashable a, Ord a, Ord c, Num c)
         => (a -> HashSet a) -- adjacencies in graph
         -> (a -> a -> c)    -- distance function
         -> (a -> c)         -- heuristic distance to goal
         -> (a -> Bool)      -- goal
         -> a                -- starting vertex
         -> AStar HashSet HashPSQ HashMap a c -- final state

runAStar graph dist heur goal = runAStarG graph dist heur goal . aStarInit

runAStarG :: (Num c, Ord a, Ord c, GSet s a, GMap m a, GQueue q a c)
          => (a -> s a)     -- adjacencies in graph
         -> (a -> a -> c)   -- distance function
         -> (a -> c)        -- heuristic distance to goal
         -> (a -> Bool)     -- goal
         -> AStar s q m a c -- initial state
         -> AStar s q m a c -- final state

runAStarG successors dist heur goal starting = aStar' starting
  where aStar' s
          = case minView (waiting s) of
              Nothing            -> s
              Just (x, _,  _, w') ->
                if goal x
                  then s { end = Just x }
                  else aStar' $ foldl' (expand x)
                                       (s { waiting = w',
                                            visited = add x (visited s)})
                                       (successors x `difference` visited s)
        expand x s y
          = let v = (score s `get` x) + dist x y
            in case lookup y (waiting s) of
                 Nothing -> link x y v
                              (s { memoHeur
                                     = insert y (heur y) (memoHeur s) })
                 Just _  -> if v < (score s `get` y)
                              then link x y v s
                              else s
        link x y v s
          = let h = v + (memoHeur s `get` y)
            in s { cameFrom = insert  y x (cameFrom s)
                 , score    = insert  y v (score s)
                 , waiting  = enqueue y h (waiting s)
                 }

-- | This function computes an optimal (minimal distance) path through a graph in a best-first fashion,
-- starting from a given starting point.
aStar :: (Hashable a, Ord a, Ord c, Num c) =>
         (a -> HashSet a)     -- ^ The graph we are searching through, given as a function from vertices
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

runAStarM :: forall a c m. (Monad m, Hashable a, Ord a, Ord c, Num c) =>
          (a -> m (HashSet a))   -- adjacencies in graph
          -> (a -> a -> m c) -- distance function
          -> (a -> m c)      -- heuristic distance to goal
          -> (a -> m Bool)   -- goal
          -> a               -- starting vertex
          -> m (AStar HashSet HashPSQ HashMap a c)   -- final state

runAStarM graph dist heur goal start = aStar' (aStarInit start :: AStar HashSet HashPSQ HM.HashMap a c)
  where aStar' s
          = case minView (waiting s) of
              Nothing            -> return s
              Just (x, _,  _, w') ->
                do g <- goal x
                   if g then return (s { end = Just x })
                        else do ns <- graph x
                                u <- foldM (expand x)
                                           (s { waiting = w',
                                                visited = add x (visited s)})
                                           (toList (ns `difference` visited s))
                                aStar' u
        expand x s y
          = do d <- dist x y
               let v = (score s `get` x) + d
               case lookup y (waiting s) of
                 Nothing -> do h <- heur y
                               return (link x y v (s { memoHeur = insert y h (memoHeur s) }))
                 Just _  -> return $ if v < score s `get` y
                                        then link x y v s
                                        else s
        link x y v s
           = s { cameFrom = insert y x (cameFrom s),
                 score    = insert y v (score s),
                 waiting  = enqueue y (v + (memoHeur s `get` y)) (waiting s) }

-- | This function computes an optimal (minimal distance) path through a graph in a best-first fashion,
-- starting from a given starting point.
aStarM :: (Monad m, Hashable a, Ord a, Ord c, Num c) =>
         (a -> m (HashSet a))   -- ^ The graph we are searching through, given as a function from vertices
                            -- to their neighbours.
         -> (a -> a -> m c) -- ^ Distance function between neighbouring vertices of the graph. This will
                            -- never be applied to vertices that are not neighbours, so may be undefined
                            -- on pairs that are not neighbours in the graph.
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
aStarOrd :: forall a c. (Ord a, Ord c, Num c) =>
         (a -> S.Set a)     -- ^ The graph we are searching through, given as a function from vertices
                          -- to their neighbours.
         -> (a -> a -> c) -- ^ Distance function between neighbouring vertices of the graph. This will
                          -- never be applied to vertices that are not neighbours, so may be undefined
                          -- on pairs that are not neighbours in the graph.
         -> (a -> c)      -- ^ Heuristic distance to the (nearest) goal. This should never overestimate the
                          -- distance, or else the path found may not be minimal.
         -> (a -> Bool)   -- ^ The goal, specified as a boolean predicate on vertices.
         -> a             -- ^ The vertex to start searching from.
         -> Maybe [a]     -- ^ An optimal path, if any path exists. This excludes the starting vertex.
aStarOrd graph dist heur goal start
  = unwindState start
  . runAStarG graph dist heur goal
  $ (aStarInit start :: AStar S.Set OrdPSQ M.Map a c)

unwindState :: (GMap m a, Eq a) => a -> AStar s q m a c -> Maybe [a]
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
    points (x, y) = [x, y]

instance (Real n) => CartesianCoordinate (n, n, n) n where
    points (x, y, z) = [x, y, z]

instance (Real n) => CartesianCoordinate (n, n, n, n) n where
    points (x, y, z, w) = [x, y, z, w]
    
-- general purpose function for computing a straight-line distance
-- between two points on a Cartesian plane of an arbitrary number of dimensions
euclideanDistance :: (CartesianCoordinate c n, Real n, Floating b) => c -> c -> b
euclideanDistance (points -> ps) (points -> qs)
  = sqrt . sum . map ((^ (2 :: Int)) . realToFrac . abs) $ zipWith subtract ps qs

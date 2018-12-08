{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections #-}

import qualified Data.OrdPSQ as PSQ
import Data.Semigroup
import Control.Monad.State.Strict as State
import qualified Data.Map.Strict as M
import           Data.Foldable (foldl', foldl,toList)
import qualified Data.Set as S
import qualified Data.List as L
import Data.Maybe
import Control.Applicative

type Node = Char -- technically can be any Ord, but this puzzle only requires chars
type DependencyGraph = M.Map Node (S.Set Node)

main :: IO ()
main = do
  deps <- buildG . fmap parseDep . lines <$> getContents
  putStrLn "Topological sort:"
  putStrLn (topologicalSort deps)
  putStrLn "Parallel execution:"
  print (parallelWork 60 4 deps)

-- simplistic parsing, because we only have to deal with good input
parseDep :: String -> (Node,Node)
parseDep s = let (a:rst) = drop prelude s
                 (b:_) = drop middle rst
              in (a,b)
  where
    prelude = length ("Step " :: String)
    middle = length (" must be finished before step " :: String)

stepCost :: Int -> Node -> Int
stepCost k n = k + (fromEnum n - fromEnum 'A') + 1

data PState = PState
  { sBlocking :: DependencyGraph
  , sAvailable :: [Node]
  , sComplete :: [Node]
  , sWorkers :: [(Int, Node)]
  , sCapacity :: Int
  } deriving Show

parallelWork :: Int -> Int -> DependencyGraph -> (Int, [Node])
parallelWork k elves dg = State.evalState (tick 0) PState { sBlocking = dg
                                                          , sAvailable = takeUnblocked dg
                                                          , sComplete = mempty
                                                          , sWorkers = []
                                                          , sCapacity = elves + 1
                                                          }
  where
    tick time = do
      reapDone time
      allocateIdle time
      mt <- State.gets nextJump 
      case mt of
        Nothing -> State.gets ((time,) . sComplete)
        Just t -> tick t
 
    reapDone t = do
      ws <- State.gets sWorkers
      let completed = fmap snd $ filter ((<= t) . fst) ws
      State.modify (markCompleted completed)

    allocateIdle now = do
      s <- State.get
      let 
          pending = length           $ sWorkers s
          idle    = subtract pending $ sCapacity s
          ignore = S.fromList (sComplete s) <> S.fromList (snd <$> sWorkers s)
          ws = getWorkers k now ignore idle (sAvailable s)
      State.put $ s { sWorkers = ws <> sWorkers s
                    , sAvailable = drop (length ws) (sAvailable s)
                    }

markCompleted ns s =
  let g = foldr M.delete (sBlocking s) ns
      stillBlocked = M.keysSet $ revG g
      ws = filter (not . (`elem` ns) . snd) (sWorkers s)
      newlyAvailable = do
        n        <- ns
        unlocked <- maybe mempty S.toList $ M.lookup n (sBlocking s)
        guard (S.notMember unlocked stillBlocked)
        return unlocked
   in s { sBlocking  = g
        , sAvailable = L.sort (sAvailable s <> newlyAvailable)
        , sComplete  = sComplete s <> ns
        , sWorkers   = ws
        }

nextJump s = case sWorkers s of
  [] -> Nothing
  ws -> Just $ minimum (fst <$> ws)

getWorkers k now ignore capacity jobs =
  [ (now + stepCost k n, n) | n <- take capacity jobs , S.notMember n ignore ]

takeUnblocked :: DependencyGraph -> [Node]
takeUnblocked g = filter (not . blocked)
                $ topologicalSort g
  where
    waiters = waiting g
    blocked n = S.member n waiters

topologicalSort :: DependencyGraph -> [Node]
topologicalSort dg = State.evalState go initState
  where
    blockedBy = revG dg
    initState = (mempty
                , insertAll PSQ.empty
                   (M.keysSet dg `S.difference` M.keysSet blockedBy)
                )
    blocked done t = any (flip S.notMember done)
                   . fromMaybe mempty
                   $ M.lookup t blockedBy
    go = do
      (done, q) <- State.get
      case PSQ.minView q of
        Nothing -> return []
        Just (t, _, _, q') -> do
          let complete = S.insert t done
              pending = insertAll q'
                      . filter (not . blocked complete)
                      . filter (flip S.notMember complete)
                      . maybe mempty S.toList
                      $ M.lookup t dg
          State.put (complete, pending)
          (t:) <$> go

insertAll :: (Foldable t, Ord a) => PSQ.OrdPSQ a a a -> t a -> PSQ.OrdPSQ a a a
insertAll = foldl $ \q t -> PSQ.insert t t t q

revG :: DependencyGraph -> DependencyGraph
revG = M.fromListWith (<>)
     . (>>= \(blocker, blockees) -> [(blocked, S.singleton blocker)
                                    | blocked <- S.toList blockees])
     . M.toList

waiting :: DependencyGraph -> S.Set Node
waiting = mconcat . M.elems

buildG :: [(Node,Node)] -> DependencyGraph
buildG = M.fromListWith (<>) . fmap (fmap S.singleton)

exampleNodes :: [(Node,Node)]
exampleNodes = 
 [('C', 'A')
 ,('C', 'F')
 ,('A', 'B')
 ,('A', 'D')
 ,('B', 'E')
 ,('D', 'E')
 ,('F', 'E')
 ]

-- with three workers, should take 10 steps
-- With 3 workers, should execute as:
-- Second   Worker 1  Worker 2 Worker 3   Done
-- 0        A         B        C          
-- 1        D         B        C          A 
-- 2        D         .        C          AB
-- 3        D         .        .          ABC
-- 4        D         .        .          ABC
-- 5        E         .        .          ABCD
-- 6        E         .        .          ABCD
-- 7        E         .        .          ABCD
-- 8        E         .        .          ABCD
-- 9        E         .        .          ABCD
-- 10       .         .        .          ABCDE
example2 :: [(Node,Node)]
example2 =
  [('A','E')
  ,('B','E')
  ,('C','E')
  ,('D','E')
  ]

-- in serial: should == sum (fmap stepCost 0) "ABCDE"
exampleSerial :: [(Node, Node)]
exampleSerial = [('A', 'B'), ('B', 'C'), ('C', 'D'), ('D', 'E')]

-- in parallel:
-- With 3 workers, should execute as:
-- Second   Worker 1  Worker 2 Worker 3   Done
-- 0        A         C        F          
-- 1        E         C        F          A 
-- 2        E         C        F          A
-- 3        E         D        F          AC
-- 4        E         D        F          AC
-- 5        E         D        F          AC
-- 6        .         D        B          ACEF
-- 7        .         .        B          ACEFD
-- 8        .         .        .          ACEFDB
exampleParallel :: [(Node, Node)]
exampleParallel = [('A', 'E')
                  ,('C', 'D')
                  ,('F', 'B')
                  ]



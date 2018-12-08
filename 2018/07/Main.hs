{-# LANGUAGE FlexibleContexts #-}

import qualified Data.OrdPSQ as PSQ
import Data.Semigroup
import Control.Monad.State.Strict as State
import qualified Data.Map.Strict as M
import           Data.Foldable (foldl', foldl,toList)
import qualified Data.Set as S
import qualified Data.List as L
import Data.Maybe
import Control.Applicative
import qualified Debug.Trace as Debug

type Node = Char -- technically can be any Ord, but this puzzle only requires chars
type DependencyGraph = M.Map Node (S.Set Node)

main :: IO ()
main = do
  deps <- buildG . fmap parseDep . lines <$> getContents
  putStrLn "Topological sort:"
  putStrLn (topologicalSort deps)
  putStrLn "Parallel execution:"
  print (parallelWork 60 4 deps)
  print (parallelWork2 60 4 deps)

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

data Elf = Idle
         | Working Int Node
         deriving (Show)

data PState = PState
  { sBlocking :: DependencyGraph
  , sAvailable :: [Node]
  , sComplete :: [Node]
  , sElapsed :: Int
  , sWorkers :: [(Int, Node)]
  , sCapacity :: Int
  } deriving Show

parallelWork2 :: Int -> Int -> DependencyGraph -> (Int, [Node])
parallelWork2 k elves dg = State.evalState tick initState
  where
    tick = do
      s <- State.get
      Debug.traceShow (sElapsed s, sWorkers s) $ return ()
      reapDone
      allocateIdle
      jumpAhead
    jumpAhead = do
      ws <- State.gets sWorkers
      if null ws
        then State.gets      $ \s -> (sElapsed s, sComplete s)
        else do State.modify $ \s -> s { sElapsed = nextTick ws }
                tick
    getWorkers now ignore capacity jobs =
      [(completesAt now n, n) | n <- take capacity jobs
                              , S.notMember n ignore
                              ]
    completesAt now n = now + stepCost k n
    initState = PState { sBlocking = dg
                       , sAvailable = takeUnblocked dg
                       , sComplete = mempty
                       , sWorkers = []
                       , sElapsed = 0
                       , sCapacity = elves + 1
                       }
    nextTick = minimum . fmap fst
    reapDone = do
      t <- State.gets sElapsed
      ws <- State.gets sWorkers
      let completed = fmap snd $ filter ((<= t) . fst) ws
      markCompleted completed
    markCompleted ns = State.modify $ \s ->
      let g = foldr M.delete (sBlocking s) ns
          rg = revG g
       in s { sBlocking = g
            , sAvailable = L.sort $ mconcat [ sAvailable s
                                            , ns >>= \n -> filter (not . flip M.member rg)
                                                           . maybe mempty S.toList
                                                           $ M.lookup n (sBlocking s)
                                            ]
            , sComplete = sComplete s <> ns
            , sWorkers = filter (not . (`elem` ns) . snd) (sWorkers s)
            }
    allocateIdle = do
      s <- State.get
      let 
          pending = length $ sWorkers s
          idle    = Debug.traceShowId $ subtract pending $ sCapacity s
          jobs    = Debug.traceShowId $ sAvailable s
          ignore = S.fromList (sComplete s) <> S.fromList (snd <$> sWorkers s)
          ws = getWorkers (sElapsed s) ignore idle jobs
      State.put $ s { sWorkers = ws <> sWorkers s
                    , sAvailable = drop (length ws) (sAvailable s)
                    }

parallelWork :: Int -> Int -> DependencyGraph -> (Int, [Node])
parallelWork k elves dg
  = let (ws,ps) = getWorkers mempty (elves + 1) (takeUnblocked dg)
     in tick dg ps ws 0 []
  where
    getWorkers processing capacity jobs =
      let accepted   = filter (flip S.notMember processing) $ take capacity jobs
          ws         = startWork k <$> accepted
          unemployed = replicate (capacity - length accepted) Idle
       in (ws <> unemployed, S.fromList accepted)
    tick g processing workers timeElapsed res =
      let (complete, idle, working) = Debug.traceShow (timeElapsed, workers) $ runWorkers workers
          t        = succ timeElapsed
          r        = res ++ complete
          g'       = foldr M.delete g complete
          available = if M.null g' then terminals g else takeUnblocked g'
          processing' = foldr S.delete processing complete
       in case (idle, available) of
         (n, [])    -> if null working
                       then (t, r) -- nothing to do, nothing happening, we are done
                       else tick g' processing' (replicate n Idle <> working) t r
         (0, _)     -> tick g' processing' working t r -- every resource is busy
         (cap,jobs) -> -- allocate idle resources, then continue
           let (ws,ps) = getWorkers processing' cap jobs
            in tick g' (processing <> ps) (working <> ws) t r

startWork :: Int -> Node -> Elf
startWork k n = Working (stepCost k n - 1) n

runWorkers :: [Elf] -> ([Node], Int, [Elf])
runWorkers = foldl' go ([], 0, [])
  where
    go (ns, n, acc) elf = case elf of
      Idle        -> (ns,   n + 1, acc)
      Working 0 x -> (x:ns, n + 1, acc)
      Working t x -> (ns,   n,     Working (t - 1) x : acc)


takeUnblocked :: DependencyGraph -> [Node]
takeUnblocked g = filter (not . blocked)
                $ topologicalSort g
  where
    waiters = waiting g
    blocked n = S.member n waiters

terminals :: DependencyGraph -> [Node]
terminals = takeUnblocked . revG

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

allNodes :: DependencyGraph -> S.Set Node
allNodes g = waiting g <> M.keysSet g

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
-- 6        .         D        F          ACE
-- 7        .         .        B          ACEF
-- 8        .         .        B          ACEF
-- 9        .         .        .          ACEFB
exampleParallel :: [(Node, Node)]
exampleParallel = [('A', 'E')
                  ,('C', 'D')
                  ,('F', 'B')
                  ]



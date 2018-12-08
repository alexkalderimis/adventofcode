import qualified Data.OrdPSQ as PSQ
import Data.Semigroup
import Control.Monad.State.Strict as State
import qualified Data.Map.Strict as M
import           Data.Foldable (foldl', foldl,toList)
import qualified Data.Set as S
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
  print (parallelWork 60 5 deps)

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

parallelWork :: Int -> Int -> DependencyGraph -> (Int, [Node])
parallelWork k elves dg
  = tick dg (getWorkers mempty (elves + 1) (takeUnblocked dg)) 0 []
  where
    getWorkers processing capacity jobs =
      let accepted = filter (flip S.notMember processing) $ take capacity jobs
          ws = startWork k <$> accepted
          unemployed = replicate (capacity - length accepted) Idle
       in ws <> unemployed
    tick g workers timeElapsed res =
      let (ns, idle, working) = runWorkers workers
          t = succ timeElapsed
          r = res ++ ns
          g' = foldr M.delete g ns
          released = if M.null g' then terminals g else []
       in case (idle, takeUnblocked g' <> released) of
         (n, [])    -> if null working
                       then (t, r) -- nothing to do, nothing happening, we are done
                       else tick g' (replicate n Idle <> working) t r
         (0, _)     -> tick g' working t r -- every resource is busy
         (cap,jobs) -> -- allocate idle resources, then continue
           let processing = S.fromList [x | Working _ x <- working]
            in tick g' (working <> getWorkers processing cap jobs) t r

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

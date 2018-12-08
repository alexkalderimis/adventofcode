import qualified Data.OrdPSQ as PSQ
import Data.Semigroup
import Control.Monad.State.Strict as State
import qualified Data.Map.Strict as M
import           Data.Foldable (foldl,toList)
import qualified Data.Set as S
import Data.Maybe
import Control.Applicative

type Node = Char -- technically can be any Ord, but this puzzle only requires chars
type DependencyGraph = M.Map Node (S.Set Node)

main :: IO ()
main = do
  deps <- buildG . fmap parseDep . lines <$> getContents
  putStrLn (topologicalSort deps)

-- simplistic parsing, because we only have to deal with good input
parseDep :: String -> (Node,Node)
parseDep s = let (a:rst) = drop prelude s
                 (b:_) = drop middle rst
              in (a,b)
  where
    prelude = length ("Step " :: String)
    middle = length (" must be finished before step " :: String)

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

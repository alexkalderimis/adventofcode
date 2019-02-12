{-# LANGUAGE OverloadedStrings #-}

import           Control.Applicative.Combinators
import           Data.Attoparsec.Text            (decimal)
import qualified Data.Attoparsec.Text            as A
import qualified Data.List                       as L
import           Data.Map.Strict                 (Map)
import qualified Data.Map.Strict                 as M
import           Data.Monoid
import           Data.Ord
import           Data.Set                        (Set)
import qualified Data.Set                        as S
import           Data.Text                       (Text)
import qualified Data.Text                       as T
import           Data.Tree                       (Forest, Tree (..))
import           Text.Parser.Char                (newline)

import           Elves
import           Elves.Advent

type Place = Text
type Distance = Sum Int
type DistanceTable = Map Place (Map Place Distance)
type Path = ([Place], Distance)

main :: IO ()
main = day 9 parser (travel minPath) (travel maxPath) test
  where
    travel f distances = print . f
                       . pathsFrom distances Nothing
                       $ places distances

test = do
  describe "example" $ do
    let paths = do t <- parseOnly parser exampleDistanceTable
                   pure (pathsFrom t Nothing (places t))
    describe "pt1" $ do
      it "calculates the correct minimal path" $ do
         fmap (snd . minPath) paths `shouldBe` Right 605
    describe "pt2" $ do
      it "calculates the correct maximal path" $ do
         fmap (snd . maxPath) paths `shouldBe` Right 982

parser :: Parser DistanceTable
parser = fmap (M.unionsWith (<>)) (edge `sepBy` newline)
  where
    place = A.takeWhile (/= ' ')
    edge = do
      a <- place
      b <- " to " *> place
      d <- " = " *> (Sum <$> decimal)
      pure $ M.fromList [(a, M.singleton b d), (b, M.singleton a d)]

exampleDistanceTable = T.unlines
  ["London to Dublin = 464"
  ,"London to Belfast = 518"
  ,"Dublin to Belfast = 141"
  ]

places :: DistanceTable -> Set Place
places = M.keysSet

edgesFrom :: Place -> DistanceTable -> [(Place, Distance)]
edgesFrom = (maybe [] M.toList .) . M.lookup

pathsFrom :: DistanceTable -> Maybe Place -> Set Place -> Forest Path
pathsFrom distances origin unvisited = do
  (place,cost) <- edges
  let es = pathsFrom distances (pure place) (S.delete place unvisited)
  pure $ Node ([place],cost) es
  where
    notVisited = flip S.member unvisited . fst
    edges = case origin of
              Nothing -> zip (S.toList . places $ distances) (repeat 0)
              Just p  -> filter notVisited $ edgesFrom p distances

minPath = bestPath id
maxPath = bestPath Down

bestPath :: Ord b => (Distance -> b) -> Forest Path -> Path
bestPath order = go
  where
    go []     = mempty
    go forest = L.minimumBy (comparing (order . snd))
              $ fmap (collapseForest go) forest


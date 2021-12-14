{-# LANGUAGE OverloadedStrings #-}
import qualified Data.Foldable as F
import qualified Data.List as L
import qualified Data.Text as T
import           Data.Text (Text)
import qualified Data.Attoparsec.Text as A
import           Control.Applicative.Combinators
import           Text.Parser.Char (newline, text)
import           Data.Tree
import qualified Data.Map.Strict as M
import           Data.Map.Strict (Map)
import qualified Data.List.NonEmpty               as NE
import Data.List.NonEmpty (NonEmpty)
import qualified Data.Set as Set
import Data.Set (Set)
import Data.Char (isLetter)

import Elves
import Elves.Advent
import Elves.Clique (leaves, searchGraph)
import Elves.Map

data PathNode
  = Minor !Text
  | Major !Text
  deriving (Eq, Ord)

instance Show PathNode where
  show (Major t) = T.unpack t
  show (Minor t) = T.unpack t

type Restriction = Path -> PathNode -> Bool
type Paths = Forest Path
type Edge = (PathNode, PathNode)
type Edges = Map PathNode (Set PathNode)

-- trading space for time - the set saves repeated O(n) reads of the path
-- we only need to record the minor nodes we have seen
-- this keeps the set smaller and improves query time.
data Path = Path { getPath :: !(NonEmpty PathNode), getSeen :: !(Set Text) }
  deriving (Eq)

instance Show Path where
  show = L.intercalate "->" . fmap show . F.toList . getPath

instance Semigroup Path where
  a <> b = Path (getPath a <> getPath b) (getSeen a <> getSeen b)

main :: IO ()
main = day 12 parser pt1 pt2 test
  where
    pt1 = totalPaths (not .* minorLoop)
    pt2 = totalPaths (not .* multipleMinorLoops)
    totalPaths r = print . length . allPaths (Minor "start") (Minor "end") r

test = do
  let path ns = let ns' = fmap majorOrMinor ns
                 in Path (NE.fromList $ reverse ns') (Set.fromList [t | Minor t <- ns'])
      allPaths' = allPaths (Minor "start") (Minor "end")

  describe "minorLoop" $ do
    it "knows that we cannot visit a minor node again" $ do
      minorLoop (path ["A", "B", "c", "D"]) (Minor "c") `shouldBe` True
    it "knows that we can visit a major node again" $ do
      minorLoop (path ["A", "B", "c", "D"]) (Major "B") `shouldBe` False

  describe "multipleMinorLoops" $ do
    it "one loop OK" $ do
      multipleMinorLoops (path ["A", "B", "c", "D"]) (Minor "c") `shouldBe` False
    it "no loops OK" $ do
      multipleMinorLoops (path ["A", "B", "c", "D"]) (Major "B") `shouldBe` False
    it "more loops bad" $ do
      multipleMinorLoops (path ["A", "b", "c", "b", "D"]) (Minor "c") `shouldBe` True

  describe "big map" $ do
    let input = [ "fs-end"
                , "he-DX"
                , "fs-he"
                , "start-DX"
                , "pj-DX"
                , "end-zg"
                , "zg-sl"
                , "zg-pj"
                , "pj-he"
                , "RW-he"
                , "fs-DX"
                , "pj-RW"
                , "zg-RW"
                , "start-pj"
                , "he-WI"
                , "zg-he"
                , "pj-fs"
                , "start-RW"
                ]
        Right es = parseOnly parser (T.intercalate "\n" input)
    it "knows there are 226 paths" $ do
      length (allPaths' (not .* minorLoop) es) `shouldBe` 226
    it "finds the right paths, pt2" $ do
      length (allPaths' (not .* multipleMinorLoops) es) `shouldBe` 3509

  describe "larger map" $ do
    let input = [ "dc-end"
                , "HN-start"
                , "start-kj"
                , "dc-start"
                , "dc-HN"
                , "LN-dc"
                , "HN-end"
                , "kj-sa"
                , "kj-HN"
                , "kj-dc"
                ]
        Right es = parseOnly parser (T.intercalate "\n" input)

    it "finds the right paths" $ do
      let paths = [ path ["start", "HN", "dc", "HN", "end"] 
                  , path ["start", "HN", "dc", "HN", "kj", "HN", "end"]
                  , path ["start", "HN", "dc", "end"]
                  , path ["start", "HN", "dc", "kj", "HN", "end"]
                  , path ["start", "HN", "end"]
                  , path ["start", "HN", "kj", "HN", "dc", "HN", "end"]
                  , path ["start", "HN", "kj", "HN", "dc", "end"]
                  , path ["start", "HN", "kj", "HN", "end"]
                  , path ["start", "HN", "kj", "dc", "HN", "end"]
                  , path ["start", "HN", "kj", "dc", "end"]
                  , path ["start", "dc", "HN", "end"]
                  , path ["start", "dc", "HN", "kj", "HN", "end"]
                  , path ["start", "dc", "end"]
                  , path ["start", "dc", "kj", "HN", "end"]
                  , path ["start", "kj", "HN", "dc", "HN", "end"]
                  , path ["start", "kj", "HN", "dc", "end"]
                  , path ["start", "kj", "HN", "end"]
                  , path ["start", "kj", "dc", "HN", "end"]
                  , path ["start", "kj", "dc", "end"]
                  ]
      allPaths' (not .* minorLoop) es `shouldMatchList` paths
    it "finds the right paths, pt2" $ do
      length (allPaths' (not .* multipleMinorLoops) es) `shouldBe` 103

  describe "simple map" $ do
    let input = [ "start-A"
                 , "start-b"
                 , "A-c"
                 , "A-b"
                 , "b-d"
                 , "A-end"
                 , "b-end"
                 ]
        Right es = parseOnly parser (T.intercalate "\n" input)

    it "finds the right paths" $ do
      let paths = [ path ["start", "A", "b", "A", "c", "A", "end"]
                  , path ["start", "A", "b", "A", "end"]
                  , path ["start", "A", "b", "end"]
                  , path ["start", "A", "c", "A", "b", "A", "end"]
                  , path ["start", "A", "c", "A", "b", "end"]
                  , path ["start", "A", "c", "A", "end"]
                  , path ["start", "A", "end"]
                  , path ["start", "b", "A", "c", "A", "end"]
                  , path ["start", "b", "A", "end"]
                  , path ["start", "b", "end"]
                  ]
      allPaths' (not .* minorLoop) es `shouldMatchList` paths
    it "finds the right paths, pt2" $ do
      length (allPaths' (not .* multipleMinorLoops) es) `shouldBe` 36

parser :: Parser Edges
parser = fmap edges . flip sepBy1 newline $ do
  a <- majorOrMinor <$> A.takeWhile1 isLetter
  text "-"
  b <- majorOrMinor <$> A.takeWhile1 isLetter
  pure (a, b)

majorOrMinor :: Text -> PathNode
majorOrMinor t = if T.toLower t == t then Minor t else Major t

allPaths :: PathNode -> PathNode -> Restriction -> Edges -> [Path]
allPaths start end r edges 
  = filter (atGoal . getPath)
  . leaves
  $ searchGraph f [mkPath start]
  where
    atGoal p = NE.head p == end
    outEdges = F.toList . withDefault Set.empty edges
    currentNode = NE.head . getPath
    f p = fmap ((<> p) . mkPath)
          . filter (r p)
          . outEdges
          $ currentNode p

mkPath :: PathNode -> Path
mkPath n = Path (pure n) (Set.fromList [t | Minor t <- [n]])

multipleMinorLoops :: Path -> PathNode -> Bool
multipleMinorLoops p n =
  let newLoop      = minorLoop p n
      previousLoop = length [() | Minor{} <- F.toList (getPath p)] /= Set.size (getSeen p)
  in newLoop && previousLoop

minorLoop :: Path -> PathNode -> Bool
minorLoop _ (Major _) = False
minorLoop p (Minor t) = Set.member t (getSeen p)

edges :: [Edge] -> Edges
edges es = M.unionsWith (<>) $ do
  (a, b) <- es
  pure . M.fromList . fmap (fmap Set.singleton)
                    . filter ((/= Minor "start") . snd)
                    . filter ((/= Minor "end") . fst)
                    $ [(a, b), (b, a)]

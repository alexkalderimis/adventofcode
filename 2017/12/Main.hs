{-# LANGUAGE OverloadedStrings #-}

import           Control.Applicative
import           Data.Attoparsec.Text    (Parser, parseOnly)
import           Data.Foldable           (foldl')
import qualified Data.Map.Strict         as M
import           Data.Maybe
import           Data.Monoid
import           Data.Set                (Set)
import qualified Data.Set                as S
import           Data.Text               (Text)
import qualified Data.Text               as Text
import qualified Data.Text.IO            as Text
import           Data.Tree
import           System.Environment
import           System.Exit
import           Text.Parser.Char
import           Text.Parser.Combinators (sepBy1)

type Person = Int
-- an infinite lazy graph from a person to all of their connections.
-- This can be followed forever, so it must be guarded with a set
-- during traversal.
type Village = Forest Person

-- first level edges in the graph, used to build the village.
type Connections = M.Map Person (Set Person)

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["pt1"]  -> run (cliqueSizeOf 0)
    ["pt2"]  -> run numberOfCliques
    _        -> die "bad arguments. Expected pt1, pt2, or test"
 where
   run f = Text.getContents >>= either die (print . f . buildVillage . buildConnections) . parseInput

-- Graph search functions:

cliqueSizeOf :: Person -> Village -> Maybe Int
cliqueSizeOf p = fmap (S.size . clique) . listToMaybe . filter ((== p) . rootLabel)

numberOfCliques :: Village -> Int
numberOfCliques = snd . foldl' go (mempty, 0)
  where
    go (seen, n) node | S.member (rootLabel node) seen = (seen, n)
    go (seen, n) node = (seen <> clique node, n + 1)

clique :: Tree Person -> Set Person
clique = go mempty
  where
    go seen (Node p _)  | S.member p seen = seen
    go seen (Node p fs) = foldl' go (S.insert p seen) fs

-- helpers for turning input into a search graph
buildVillage :: Connections -> Village
buildVillage conns = fmap node (M.keys conns)
  where
    node p = Node p (fmap node . S.toList . fromMaybe mempty $ M.lookup p conns)

buildConnections :: [(Person, [Person])] -> Connections
buildConnections = M.fromListWith (<>) . fmap (fmap S.fromList)

-- input parsing
linksP :: Parser (Person, [Person])
linksP = (,) <$> pp <*> (string " <-> " *> (pp `sepBy1` string ", "))

pp :: Parser Person
pp = read <$> some digit

parseInput :: Text -> Either String [(Person, [Person])]
parseInput = parseOnly (linksP `sepBy1` newline)

-- example from the instructions
exampleVillage :: Text
exampleVillage = Text.unlines
  [ "0 <-> 2"
  , "1 <-> 1"
  , "2 <-> 0, 3, 4"
  , "3 <-> 2, 4"
  , "4 <-> 2, 3, 6"
  , "5 <-> 6"
  , "6 <-> 4, 5"
  ]

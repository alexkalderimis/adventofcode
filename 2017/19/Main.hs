{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Text as Text
import qualified Data.Array as A
import           Text.Parser.Char        (newline)
import           Text.Parser.Combinators (choice, sepBy1)
import           Data.Attoparsec.Text    (letter)

import Elves
import Elves.Advent
import qualified Elves.Cartesian as C

data Tile
  = Named Char
  | Empty
  | Turn
  | Horizontal
  | Vertical
  deriving (Show, Eq)

newtype Diagram = Diagram (A.Array C.Location Tile) deriving (Show, Eq)

main :: IO ()
main = day 19 parser pt1 pt2 test
  where
    pt1 (Diagram _) = error "unimplemented"
    pt2 (Diagram _) = error "unimplemented"

test = do
  pure ()

parser = diagram <$> (rowP `sepBy1` newline)
  where
    rowP = some $ choice [Empty      <$ " "
                         ,Turn       <$ "+"
                         ,Horizontal <$ "-"
                         ,Vertical   <$ "|"
                         ,Named      <$> letter
                         ]

diagram []   = error "No rows"
diagram rows = Diagram $ A.listArray ((0,0), (length rows - 1, rowLen - 1)) (concat $ fmap (padTo rowLen Empty) rows)
  where rowLen = maximum (length <$> rows)

padTo n e es = take n (es ++ repeat e)

exampleDiagram = Text.unlines
  ["     |          "
  ,"     |  +--+    "
  ,"     A  |  C    "
  ," F---|----E|--+ "
  ,"     |  |  |  D "
  ,"     +B-+  +--+ "
  ," "
  ]


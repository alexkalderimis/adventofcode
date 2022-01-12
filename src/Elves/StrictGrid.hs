{-# LANGUAGE FlexibleContexts #-}

module Elves.StrictGrid (
  Coordinate(..), Row(..), Col(..),
  move, accelerate, gridP, nextCoords, draw
  ) where

import qualified Data.List.Extra as L
import           Control.Applicative.Combinators (sepBy1, some)
import           Text.Parser.Char (newline)
import Data.Attoparsec.Text (Parser)
import qualified Data.Array.IArray as Array
import           Data.Array.IArray (IArray, Ix)

import Elves.Coord.Strict (Coordinate(..), Row(..), Col(..))

data Delta = Delta
  { dy :: {-# UNPACK #-} !Int
  , dx :: {-# UNPACK #-} !Int
  } deriving (Show, Eq, Ord)

data Acceleration = Accel { ddy :: Int -> Int, ddx :: Int -> Int }

move :: Coordinate -> Delta -> Coordinate
move c d = Coord (Row $ getRow (row c) + dy d) (Col $ getCol (col c) + dx d)

accelerate :: Acceleration -> Delta -> Delta
accelerate (Accel ddy ddx) d = d { dy = ddy (dy d), dx = ddx (dx d) }

gridP :: IArray a e => Parser e -> Parser (a Coordinate e)
gridP p = do
  lines <- some p `sepBy1` newline
  let rows = length lines - 1
      cols = length (head lines) - 1
      bs = (Coord (Row 0) (Col 0), Coord (Row rows) (Col cols))
  pure . Array.array bs $ do (i, line) <- zip [0..] lines
                             (j, x) <- zip [0..] line
                             pure (Coord (Row i) (Col j), x)

nextCoords :: Bool -> (Coordinate, Coordinate) -> Coordinate -> [Coordinate]
nextCoords includeDiagonals bs (Coord r c) =
  let straights = [ Coord (pred r) c
                  , Coord (succ r) c
                  , Coord r (pred c)
                  , Coord r (succ c)
                  ]
      diagnonals = [ Coord (pred r) (pred c)
                   , Coord (pred r) (succ c)
                   , Coord (succ r) (pred c)
                   , Coord (succ r) (succ c)
                   ]
   in filter (Array.inRange bs) $ mconcat [straights, if includeDiagonals then diagnonals else []]

draw :: IArray a Char => a Coordinate Char -> String
draw grid = let (lb, ub) = Array.bounds grid
                w = getCol (col ub) - getCol (col lb) + 1
             in L.intercalate "\n" . L.chunksOf w $ Array.elems grid

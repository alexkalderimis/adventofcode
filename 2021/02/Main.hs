{-# LANGUAGE OverloadedStrings, GeneralizedNewtypeDeriving #-}

import qualified Data.Text as T
import qualified Data.List as L
import           Data.Text (Text)
import qualified Data.Attoparsec.Text as A
import Text.Parser.Combinators (sepBy1, choice)
import Text.Parser.Char (newline, text)

import Elves.Advent

newtype Distance = Distance { getDistance :: Int }
  deriving (Show, Eq, Integral, Real, Num, Ord, Enum)

data Direction = Forward | Down | Up
  deriving (Show, Eq)

data Instruction = Instruction !Direction !Distance
  deriving (Show, Eq)

data Location = Location { horizontal :: !Int, depth :: !Int, aim :: !Int }
  deriving (Show, Eq)

main :: IO ()
main = day 01 parser pt1 pt2 test
  where
    parser = instructionP `sepBy1` newline
    pt1 instructions = case destination instructions of
                         loc -> print (horizontal loc * depth loc)
    pt2 instructions = case destination' instructions of
                         loc -> print (horizontal loc * depth loc)

instructionP :: Parser Instruction
instructionP = Instruction <$> directionP <*> (text " " *> A.decimal)
  where
    directionP = choice [Forward <$ text "forward", Down <$ text "down", Up <$ text "up"]

test = describe "2021/02" $ do
  let exampleInput = [ "forward 5"
                     , "down 5"
                     , "forward 8"
                     , "up 3"
                     , "down 8"
                     , "forward 2"
                     ]
  let input = mapM (parseOnly instructionP) exampleInput
      clearAim d = d { aim = 0 }

  it "predicts location correctly" $ do
    fmap destination input `shouldBe` Right (Location 15 10 0)
  it "predicts location correctly, using aim" $ do
    fmap (clearAim . destination') input `shouldBe` Right (Location 15 60 0)

destination :: [Instruction] -> Location
destination = L.foldl' move (Location 0 0 0)
  where
    move loc (Instruction dir (Distance n)) = case dir of
      Up -> loc { depth = depth loc - n }
      Down -> loc { depth = depth loc + n }
      Forward -> loc { horizontal = horizontal loc + n }

destination' :: [Instruction] -> Location
destination' = L.foldl' move (Location 0 0 0)
  where
    move loc (Instruction dir (Distance n)) = case dir of
      Up -> loc { aim = aim loc - n }
      Down -> loc { aim = aim loc + n }
      Forward -> loc { horizontal = horizontal loc + n
                     , depth = depth loc + (aim loc * n)
                     }

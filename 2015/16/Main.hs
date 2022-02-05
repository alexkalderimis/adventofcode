{-# LANGUAGE OverloadedStrings #-}

import           Control.Applicative.Combinators hiding (count)
import qualified Data.Attoparsec.Text as A
import qualified Data.List as L
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Map.Strict as M
import           Data.Map.Strict (Map)
import           Text.Parser.Char (newline, text)

import Elves
import Elves.Advent

main :: IO ()
main = day 16 parser pt1 pt2 test
  where
    parser = A.sepBy1 parseAunt newline
    pt1 = find possible
    pt2 = find possiblePt2
    find f aunts = case filter (f clue . snd) aunts of
                    [] -> putStrLn "No aunt found"
                    [(n, _)] -> print n
                    as -> do putStrLn "Found several aunts:"
                             mapM_ print as

test = it "should have tests" pending

newtype Count = Count { unCount :: Maybe Int }
  deriving (Show, Eq)

none :: Count
none = Count Nothing

exactly :: Int -> Count
exactly = Count . pure

couldMatch :: (Int -> Int -> Bool) -> Count -> Count -> Bool
couldMatch f (Count clue) (Count value) = fromMaybe True (liftA2 f clue value)

data Aunt = Aunt
  { children    :: !Count
  , cats        :: !Count
  , samoyeds    :: !Count
  , pomeranians :: !Count
  , akitas      :: !Count
  , vizslas     :: !Count
  , goldfish    :: !Count
  , trees       :: !Count
  , cars        :: !Count
  , perfumes    :: !Count
  } deriving (Show, Eq)

aunt :: Aunt
aunt = Aunt none none none none none
            none none none none none

clue :: Aunt
clue = Aunt { children    = exactly 3
            , cats        = exactly 7
            , samoyeds    = exactly 2
            , pomeranians = exactly 3
            , akitas      = exactly 0
            , vizslas     = exactly 0
            , goldfish    = exactly 5
            , trees       = exactly 3
            , cars        = exactly 2
            , perfumes    = exactly 1
            }

possible :: Aunt -> Aunt -> Bool
possible clue aunt = applyClue (==) clue aunt
                         [ children, cats, samoyeds, pomeranians, akitas, vizslas
                         , goldfish, trees, cars, perfumes
                         ]

possiblePt2 :: Aunt -> Aunt -> Bool
possiblePt2 clue aunt =
  applyClue   (==) clue aunt [children, samoyeds, akitas, vizslas, cars, perfumes]
  && applyClue (<) clue aunt [cats, trees]
  && applyClue (>) clue aunt [pomeranians, goldfish]

applyClue cmp clue aunt = all (\f -> couldMatch cmp (f clue) (f aunt))

parseAunt :: Parser (Int, Aunt)
parseAunt = do
  n <- text "Sue " *> A.decimal
  text ": "
  counts <- labelledCounts
  let a = Aunt (count counts "children")
               (count counts "cats")
               (count counts "samoyeds")
               (count counts "pomeranians")
               (count counts "akitas")
               (count counts "vizslas")
               (count counts "goldfish")
               (count counts "trees")
               (count counts "cars")
               (count counts "perfumes")
  pure (n, a)
  where
    labelledCounts = M.fromList <$> A.sepBy1 labelledCount (text ", ")
    labelledCount = do label <- A.takeTill (== ':')
                       text ": "
                       n <- A.decimal
                       pure (label, n)
    count m k = Count (M.lookup k m)

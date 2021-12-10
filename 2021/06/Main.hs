{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

import qualified Data.Attoparsec.Text as A
import Text.Parser.Combinators (sepBy1)
import Text.Parser.Char (text)
import qualified Data.Array as Array
import           Data.Array (Array, (!))

import Elves
import Elves.Advent

type Days = Int
type Count = Int

main :: IO ()
main = day 06 parser pt1 pt2 test
  where
    parser = A.decimal `sepBy1` text ","
    pt1 = print . populationSizeAfter 80
    pt2 = print . populationSizeAfter 256

test = describe "populationSizeAfter" $ do
  it "can track the evolution of population founded by a single fish" $ do
    -- Initial state: (1) 3
    --
    -- After 1 day:   (1) 2
    -- After 2 days:  (1) 1
    -- After 3 days:  (1) 0
    --
    -- After 4 days:  (2) 6 8
    -- After 5 days:  (2) 5 7
    -- ...
    -- After 10 days: (2) 0 2
    --
    -- After 11 days: (3) 6 2 8
    -- After 12 days: (3) 5 1 7
    -- ..
    fmap (\n -> populationSizeAfter n [3]) [0..12]
      `shouldBe` (replicate 4 1 <> replicate 7 2 <> replicate 2 3)
  describe "for the example population" $ do
    let ages = [3, 4, 3, 1, 2]

    it "knows there are 26 after 18 days" $ do
      populationSizeAfter 18 ages `shouldBe` 26
    it "knows there are 5934 after 80 days" $ do
      populationSizeAfter 80 ages `shouldBe` 5934

populationSizeAfter :: Days -> [Days] -> Count
populationSizeAfter days ages
  | days < 1 = length ages
  | otherwise = let n = days - 1
                    a = descendents n
                 in length ages + sum [a ! (n - i) | i <- ages, i <= n]

-- an array containing number of descendents from the day of first spawning
-- This is used to memoized the recursive steps in calculating the descendents,
-- implementing dynamic programming via the lazy lookups into the array.
descendents :: Int -> Array Days Count
descendents totalTime = arr
  where
    arr = Array.listArray (0, totalTime) (fmap descendentsAt [0..totalTime])
    descendentsAt remaining
      | remaining < 0 = 0
      | otherwise = 1 + sum [ arr ! i | i <- [remaining - 7, remaining - 9], i >= 0]

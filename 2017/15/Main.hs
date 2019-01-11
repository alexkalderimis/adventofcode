{-# LANGUAGE BinaryLiterals    #-}
{-# LANGUAGE OverloadedStrings #-}

import           Data.Attoparsec.Text (string)
import           Data.Bits            ((.&.))
import qualified Data.List            as L
import           Data.Word
import           Text.Parser.Char     (digit, newline)

import           Elves
import           Elves.Advent

main :: IO ()
main = day 15 parser pt1 pt2 test
  where
    pt1 (a,b) = print $ judge $ take forty_million $ zip (generator factor_a a)
                                                         (generator factor_b b)
    pt2 (a,b) = print $ judge $ take five_million $ zip (filter (multipleOf 4) $ generator factor_a a)
                                                        (filter (multipleOf 8) $ generator factor_b b)
    parser = do a <- string "Generator A starts with " *> (read <$> some digit)
                newline
                b <- string "Generator B starts with " *> (read <$> some digit)
                newline
                return (a,b)

generator :: Word64 -> Word64 -> [Word64]
generator factor = tail . iterate ((`rem` 2147483647) . (factor *))

judge :: [(Word64,Word64)] -> Int
judge = length . filter (\(a,b) -> a .&. mask == b .&. mask)
  where mask = 0b1111111111111111

multipleOf x = (== 0) . (`mod` x)

forty_million = 40 * (10 ^ 6)
five_million = 5 * (10 ^ 6)

factor_a = 16807
factor_b = 48271

test = do
  let as = generator factor_a 65
  let bs = generator factor_b 8921
  describe "judge" $ do
    specify "there is one match in the first 5" $ do
      judge (take 5 $ zip as bs) `shouldBe` 1
    specify "there are 588 matches in the first 40 million" $ do
      judge (take forty_million $ zip as bs) `shouldBe` 588
  describe "part-2" $ do
    let pt2 n = judge (take n $ zip (filter (multipleOf 4) as) (filter (multipleOf 8) bs))
    specify "there is 1 match in the first 1056 filtered generators" $ do
      pt2 1056 `shouldBe` 1
    specify "there are 309 matches in the first 5 million filtered generators" $ do
      pt2 five_million `shouldBe` 309
  describe "generator" $ do
    describe "A" $ do
      it "has the correct values, for seed 65" $ do
        take 5 as `shouldBe` [1092455,1181022009,245556042,1744312007,1352636452]
    describe "B" $ do
      it "has the correct values, for seed 8921" $ do
        take 5 bs `shouldBe` [430625591,1233683848,1431495498,137874439,285222916]


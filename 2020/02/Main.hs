{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

import Data.Bits (xor)
import qualified Data.Text as T
import           Data.Text (Text)
import qualified Data.Attoparsec.Text as A
import Text.Parser.Char (newline)
import Text.Parser.Combinators (sepBy1)

import Elves
import Elves.Advent

data Rule = Rule { rangeStart :: Int, rangeEnd :: Int, character :: Char }
  deriving (Show, Eq)

data Password = P Text deriving (Show, Eq)

main :: IO ()
main = day 02 parser pt1 pt2 test
  where
    pt1 = putStrLn . show . length . filter (uncurry check)
    pt2 = putStrLn . show . length . filter (uncurry check2)

parser = ((,) <$> rule <*> password) `sepBy1` newline
 where
   password = fmap P (A.string ": " >> A.takeWhile1 (A.inClass "a-z"))
   rule = Rule <$> A.decimal
               <*> (A.string "-" >> A.decimal)
               <*> (A.string " " >> A.letter)

check :: Rule -> Password -> Bool
check r (P pwd) = let count = T.count (T.singleton $ character r) pwd
                   in rangeStart r <= count && count <= rangeEnd r

check2 :: Rule -> Password -> Bool
check2 Rule{..} (P pwd) =
  (at rangeStart == Just character) `xor` (at rangeEnd == Just character)
  where
    at i = case T.compareLength pwd i of
      LT -> Nothing
      _ -> pure $ T.index pwd (i - 1)

test = do
  describe "check" $ do
    it "knows that 1-3 a: abcde is valid" $ do
      check (Rule 1 3 'a') (P "abcde") `shouldBe` True
    it "knows that 1-3 z: abcde is invalid" $ do
      check (Rule 1 3 'z') (P "abcde") `shouldBe` False
    it "knows that 2-3 a: abcde is invalid" $ do
      check (Rule 2 3 'a') (P "abcde") `shouldBe` False

  describe "check2" $ do
    it "knows that 1-3 a: abcde is valid" $ do
      check2 (Rule 1 3 'a') (P "abcde") `shouldBe` True
    it "knows that 1-3 a: abade is invalid" $ do
      check2 (Rule 1 3 'a') (P "abade") `shouldBe` False
    it "knows that 1-3 a: xbade is valid" $ do
      check2 (Rule 1 3 'a') (P "xbade") `shouldBe` True
    it "knows that 2-4 d: xbade is valid" $ do
      check2 (Rule 2 4 'd') (P "xbade") `shouldBe` True
    it "knows that 1-3 z: abcde is invalid" $ do
      check2 (Rule 1 3 'z') (P "abcde") `shouldBe` False
    it "knows that 2-3 a: abcde is invalid" $ do
      check2 (Rule 2 3 'a') (P "abcde") `shouldBe` False

  describe "parser" $ do
    let input = T.unlines ["1-3 a: abcde"
                          ,"1-3 b: cdefg"
                          ,"2-9 c: ccccccccc"
                          ]
    it "parses the sample correctly" $ do
      A.parseOnly parser input `shouldBe` Right [(Rule 1 3 'a', P "abcde")
                                                ,(Rule 1 3 'b', P "cdefg")
                                                ,(Rule 2 9 'c', P "ccccccccc")
                                                ]

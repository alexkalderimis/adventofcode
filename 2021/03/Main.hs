{-# LANGUAGE OverloadedStrings, PatternSynonyms, GeneralizedNewtypeDeriving #-}

import qualified Data.Bits as B
import  Data.Bits (complement, shiftL, xor)
import qualified Data.Text as T
import qualified Data.List as L
import           Data.Text (Text)
import qualified Data.Attoparsec.Text as A
import Text.Parser.Combinators (sepBy1, choice)
import Text.Parser.Char (newline, text)

import Debug.Trace -- TODO

import Elves
import Elves.Advent
import Elves.Math.Binary (fromBinary, mkBit, mkBinary, Bit(bitIsSet), Binary(bits), zero, one)

data SearchTree = Branch !Int !Int (Maybe SearchTree) (Maybe SearchTree)
  deriving (Show)

main :: IO ()
main = day 03 parser pt1 pt2 test
  where
    parser = binaryP `sepBy1` newline
    pt1 bs = let g = gamma bs
                 e = complement g
              in print (fromBinary g * fromBinary e)
    pt2 bs = let st = buildSearchTree bs
                 ogr = oxygenGeneratorRating st
                 csr = co2ScrubberRating st
              in print (fromBinary ogr * fromBinary csr)

gamma :: [Binary] -> Binary
gamma = mkBinary . map collapse . L.transpose . map bits
  where collapse bs = mkBit $ B.popCount (mkBinary bs) >= (length bs `div` 2)

binaryP :: Parser Binary
binaryP = mkBinary <$> some bitP
  where
    bitP = choice [one <$ text "1", zero <$ text "0"]

test = describe "2021/03" $ do
  let exampleInput = [ "00100"
                     , "11110"
                     , "10110"
                     , "10111"
                     , "10101"
                     , "01111"
                     , "00111"
                     , "11100"
                     , "10000"
                     , "11001"
                     , "00010"
                     , "01010"
                     ]
  let input = mapM (parseOnly binaryP) exampleInput
      searchTree = fmap buildSearchTree input

  it "calculates gamma correctly" $ do
    fmap (fromBinary . gamma) input `shouldBe` Right 22
  it "calculates epsilon correctly" $ do
    fmap (fromBinary . complement . gamma) input `shouldBe` Right 9
  it "calculates oxygen generator rating correctly" $ do
    fmap (fromBinary . oxygenGeneratorRating) searchTree `shouldBe` Right 23
  it "calculates CO2 scrubber rating correctly" $ do
    fmap (fromBinary . co2ScrubberRating) searchTree `shouldBe` Right 10

binaryToTree :: [Bit] -> Maybe SearchTree
binaryToTree [] = Nothing
binaryToTree (b:bs) = pure $ if bitIsSet b
                      then Branch 0 1 Nothing (binaryToTree bs)
                      else Branch 1 0 (binaryToTree bs) Nothing

insert :: Maybe SearchTree -> Binary -> Maybe SearchTree
insert tree number = go tree (bits number)
  where
    go (Just (Branch zeros ones zt ot)) (b:bs)
      = pure $ if bitIsSet b
                  then Branch zeros (ones + 1) zt (go ot bs)
                  else Branch (zeros + 1) ones (go zt bs) ot
    go Nothing bs = binaryToTree bs
    go t _ = t

buildSearchTree :: [Binary] -> Maybe SearchTree
buildSearchTree [] = Nothing
buildSearchTree (b:bs) = L.foldl' insert
                                  (binaryToTree $ bits b)
                                  bs

oxygenGeneratorRating :: Maybe SearchTree -> Binary
oxygenGeneratorRating = go []
  where 
    go path Nothing = mkBinary (reverse path)
    go path (Just (Branch zeros ones zt ot)) =
      if ones >= zeros then go (one : path) ot
                       else go (zero : path) zt

co2ScrubberRating :: Maybe SearchTree -> Binary
co2ScrubberRating = go []
  where 
    go path Nothing = mkBinary (reverse path)
    go path (Just (Branch 0 ones _ ot)) = go (one : path) ot
    go path (Just (Branch zeros 0 zt _)) = go (zero : path) zt
    go path (Just (Branch zeros ones zt ot)) =
      if ones < zeros then go (one : path) ot
                      else go (zero : path) zt


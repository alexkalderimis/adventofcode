{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import qualified Data.Ix as Ix
import qualified Data.Attoparsec.Text as A
import           Control.Applicative.Combinators
import           Text.Parser.Char (text, newline)

import Elves hiding (example)
import Elves.Advent hiding (example)
import qualified Elves.RTree as RT
import qualified Elves.CountMap as CM

data Claim = Claim { claimId :: ClaimId
                   , claimedRange :: (SquareInch, SquareInch)
                   }

newtype ClaimId = ClaimId { unclaimId :: Int } deriving (Show, Eq)

type OverlappedClaims = RT.RTree SquareInch ClaimId

type SquareInch = (Int,Int)

-- a faster version for pt2 (about 75% speedup compared to using sets and maps).
main :: IO ()
main = day 2 parser pt1 pt2 spec
  where
    parser = claimP `sepBy1` newline
    pt1 = print . overlappingArea
    pt2 = mapM_ print . uncontestedClaims . overlapAllClaims
    spec = specify "we have the correct solution for the example" $ do
             example `shouldBe` Right (4, [ClaimId 3])

-- Parse claims, of the form:
-- #1 @ 1,3: 4x4
-- #2 @ 3,1: 4x4
-- #3 @ 5,5: 2x2
claimP :: Parser Claim
claimP = do
  cid    <- ClaimId <$> (text "#" >> A.decimal)
  (x, y) <- text " @ " >> intPair ","
  (w, h) <- text ": "  >> intPair "x"

  pure (Claim cid ((x,y), (x + w - 1, y + h - 1)))
  where
    intPair sep = (,) <$> A.decimal <*> (text sep >> A.decimal)

overlappingArea :: [Claim] -> Int
overlappingArea = length . CM.counting (> 1) . CM.fromList . (fmap claimedRange >=> Ix.range)

uncontestedClaims :: OverlappedClaims -> [ClaimId]
uncontestedClaims t = fmap (snd . head)
                    . filter ((== 1) . length)
                    . fmap (`RT.overlaps` t)
                    $ RT.locations t

overlapAllClaims :: [Claim] -> OverlappedClaims
overlapAllClaims = RT.fromList . fmap (claimedRange &&& claimId)

example :: Either String (Int, [ClaimId])
example = (overlappingArea &&& uncontestedClaims . overlapAllClaims)
        <$> traverse (parseOnly claimP) [ "#1 @ 1,3: 4x4" ,"#2 @ 3,1: 4x4" ,"#3 @ 5,5: 2x2" ]

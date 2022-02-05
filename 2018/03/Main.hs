{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import qualified Data.Ix as Ix
import qualified Data.Attoparsec.Text as A
import Control.Applicative
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Data.Maybe
import           Control.Applicative.Combinators
import           Text.Parser.Char (text, newline)

import Elves hiding (example)
import Elves.Advent hiding (example)

data Claim = Claim { claimId :: ClaimId
                   , claimOffset :: Offset
                   , claimSize :: Size
                   } deriving (Show)

newtype ClaimId = ClaimId { unclaimId :: Int } deriving (Show, Eq, Ord)

type OverlappedClaims = M.Map SquareInch (S.Set ClaimId)

data Offset = Offset { offsetX :: Int, offsetY :: Int } deriving Show
data Size = Size { sizeWidth :: Int, sizeHeight :: Int } deriving Show

type SquareInch = (Int,Int)

main :: IO ()
main = day 2 parser pt1 pt2 spec
  where
    parser = claimP `sepBy1` newline
    pt1 = print . overlappingArea . overlapAllClaims
    pt2 = mapM_ print . uncontestedClaims . overlapAllClaims
    spec = specify "we have the correct solution for the example" $ do
             example `shouldBe` Right (4, S.singleton (ClaimId 3))

-- Parse claims, of the form:
-- #1 @ 1,3: 4x4
-- #2 @ 3,1: 4x4
-- #3 @ 5,5: 2x2
claimP :: Parser Claim
claimP = Claim <$> claimIdP
               <*> (text " @ " >> offsetP)
               <*> (text ": " >> sizeP)

claimIdP :: Parser ClaimId
claimIdP = ClaimId <$> (text "#" >> int)

offsetP :: Parser Offset
offsetP = Offset <$> int <*> (text "," >> int)

sizeP :: Parser Size
sizeP = Size <$> int <*> (text "x" >> int)

int :: Parser Int
int = A.decimal

claimedInches :: Claim -> [SquareInch]
claimedInches = Ix.range . claimedRange

claimedRange :: Claim -> (SquareInch, SquareInch)
claimedRange claim = (lb, ub)
  where
    lb = (x, y)
    ub = (x + dx claim, y + dy claim)
    dx = subtract 1 . sizeWidth . claimSize
    dy = subtract 1 . sizeHeight . claimSize
    Offset x y = claimOffset claim

overlappingArea :: OverlappedClaims -> Int
overlappingArea = S.size . overlaps

overlaps :: OverlappedClaims -> S.Set SquareInch
overlaps = M.keysSet . M.filter ((> 1) . S.size)

uncontestedClaims :: OverlappedClaims -> S.Set ClaimId
uncontestedClaims = M.keysSet . M.filter (== 1) . foldl f mempty
  where f m = M.unionWith max m . neighbourhood
        neighbourhood set = M.fromSet (pure $ S.size set) set

overlapAllClaims :: [Claim] -> OverlappedClaims
overlapAllClaims = M.unionsWith (<>) . fmap coverage
  where coverage claim = M.fromSet (pure . S.singleton $ claimId claim)
                       . S.fromList
                       $ claimedInches claim

example :: Either String (Int, S.Set ClaimId)
example = do
  claims <- traverse (parseOnly claimP) ["#1 @ 1,3: 4x4"
                                        ,"#2 @ 3,1: 4x4"
                                        ,"#3 @ 5,5: 2x2"
                                        ]
  let allClaims = overlapAllClaims claims
  return (overlappingArea allClaims, uncontestedClaims allClaims)


{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DerivingVia #-}

import qualified Data.List as L
import           Data.Function (on)
import           Data.Monoid (Sum(..))
import qualified Data.Attoparsec.Text as A
import Text.Parser.Combinators (sepBy1)
import Text.Parser.Char (text)
import qualified Data.Array as Array
import           Data.Array (Array, (!))

import Elves
import Elves.Advent

type Position = Int
newtype Cost = Cost { fromCost :: Int }
  deriving stock Show
  deriving (Ord, Eq, Enum) via Int
  deriving (Semigroup, Monoid) via (Sum Int)

main :: IO ()
main = day 07 parser pt1 pt2 test
  where
    parser = A.decimal `sepBy1` text ","
    pt1 = print . fromCost . snd . bestPosition straightLineCost
    pt2 = print . fromCost . snd . bestPosition triangularCost

test = describe "bestPosition" $ do
  let input = [16,1,2,0,4,2,7,1,2,14]
  describe "naive cost function" $ do
    it "can solve the example problem" $ do
      bestPosition straightLineCost input `shouldBe` (2, Cost 37)
  describe "custom cost function" $ do
    it "can solve the example problem" $ do
      bestPosition triangularCost input `shouldBe` (5, Cost 168)

straightLineCost :: Position -> Position -> Cost
straightLineCost a b = Cost $ abs (a - b)

triangularCost :: Position -> Position -> Cost
triangularCost a b = let Cost d = straightLineCost a b in Cost (d * (d + 1) `div` 2)

bestPosition :: (Position -> Position -> Cost) -> [Position] -> (Position, Cost)
bestPosition f ps = L.minimumBy (compare `on` snd)
                . zip [fst bounds ..]
                . fmap mconcat
                . L.transpose
                $ map (costs f bounds) ps
  where
    bounds = (minimum ps, maximum ps)
                 
costs :: (Position -> Position -> Cost) -> (Position, Position) -> Position -> [Cost]
costs f (from, to) pos = fmap (f pos) [from .. to]

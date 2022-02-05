{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}

module Main (main) where

import           Data.Attoparsec.Text    (decimal, space)
import           Data.Monoid
import qualified Data.Text               as T
import qualified Data.List               as L
import           Text.Parser.Char        (newline, text)
import           Text.Parser.Combinators (choice, sepBy1)
import           Data.Array.ST (STUArray, runSTUArray, newArray, writeArray, readArray)
import qualified Data.Array.Unboxed as A
import           Control.Monad.ST

import           Elves
import           Elves.Advent
import           Elves.StrictGrid        (Coordinate(..), Row(..), Col(..))
import           Elves.Coord             (Bounds, expandB)

data Command = On | Off | Toggle deriving (Show, Eq)

type Region = Bounds Coordinate
type Commands = [(Region, Command)]

type HouseLights = A.UArray Coordinate Int

-- Reference implementation against a mutable unboxed array
--
-- pt1 and pt2 follow the identical algorithm, and take about 500ms
--
-- Messier than using immutable arrays, and requires explicit foralls,
-- and lazy passing of reads to avoid a space leak, but not that much
-- messier for a significant (2x) speedup.
-- 
-- Note that using an unboxed array is very important here. Using a boxed
-- array results in run times on the order of 900ms (pt1, i.e. 2x) and
-- 1200ms (pt2, i.e. 3x).
main :: IO ()
main = day 6 parser pt1 pt2 test
  where
    pt1 = print . luminosity . countLit
    pt2 = print . luminosity . getBrightness

houseBounds :: Commands -> Region
houseBounds = L.foldl1 expandB . fmap fst

-- turn off 199,133 through 461,193
-- toggle 322,558 through 977,958
-- turn on 226,196 through 599,390
parser :: Parser Commands
parser = commandP `sepBy1` newline
  where
    location = Coord <$> (fmap Row decimal <* ",") <*> fmap Col decimal
    commandP = do
      cmd <- choice [On     <$ "turn on"
                    ,Off    <$ "turn off"
                    ,Toggle <$ "toggle"
                    ]
      space
      lb <- location
      text " through "
      ub <- location
      return ((lb,ub), cmd)

test = do
  describe "exampleInput" $ do
    let Right cs = parseOnly parser exampleInput

    describe "pt1" $ do
      it "evaluates correctly" $ do
        let expected = (1000 * 1000) - 1000 - 4
        luminosity (countLit cs) `shouldBe` expected

    describe "pt2" $ do
      it "evaluates correctly" $ do
        let expected = (1000 * 1000) + 2000 - 4
        luminosity (getBrightness cs) `shouldBe` expected

  describe "collisions" $ do
    let bs = (Coord 1 1, Coord 10 10)
        cs = [(bs, Toggle)
             ,((Coord 5 5, Coord 5 5), Off)
             ,(bs, On)
             ,(bs, Toggle)
             ]
    describe "pt1" $ do
      let expected = 100 - 100
      it "calculates correctly" $ do
        luminosity (countLit cs) `shouldBe` expected
    describe "pt2" $ do
      let expected = (99 * (2 + 1 + 2)) + (1 * (2 - 1 + 1 + 2))
      it "calculates correctly" $ do
        luminosity (getBrightness cs) `shouldBe` expected

exampleInput = T.unlines
  ["turn on 0,0 through 999,999"
  ,"toggle 0,0 through 999,0"
  ,"turn off 499,499 through 500,500"
  ]

luminosity :: HouseLights -> Int
luminosity = sum . A.elems

{-# INLINE lightHouse #-}
lightHouse :: (forall s. Command -> ST s Int -> ST s Int) -> Commands -> HouseLights
lightHouse f cmds = runSTUArray $ do
  house <- newArray (houseBounds cmds) 0
  mapM_ (update house) operations
  pure house
  where
    update a (i, cmd) = f cmd (readArray a i) >>= writeArray a i
    operations        = cmds >>= \(bs, cmd) -> zip (A.range bs) (repeat cmd)

countLit :: Commands -> HouseLights
countLit = lightHouse changes
  where
    changes Off    = const (pure 0)
    changes On     = const (pure 1)
    changes Toggle = fmap (1 -)

getBrightness :: Commands -> HouseLights
getBrightness = lightHouse changes
  where
    changes cmd = fmap (adjust cmd)

    adjust Off    = max 0 . subtract 1
    adjust On     = (+ 1)
    adjust Toggle = (+ 2)

{-# LANGUAGE OverloadedStrings #-}

import           Data.Attoparsec.Text    (decimal, space)
import           Data.Monoid
import qualified Data.Text               as T
import qualified Data.List               as L
import           Text.Parser.Char        (newline, text)
import           Text.Parser.Combinators (choice, sepBy1)
import           Data.Array.Unboxed      (UArray, (//), (!))
import qualified Data.Array.Unboxed      as A

import           Elves
import           Elves.Advent
import           Elves.StrictGrid        (Coord(..), Row(..), Col(..))
import           Elves.Coord             (Bounds)

data Command = On | Off | Toggle deriving (Show, Eq)

type Region = Bounds Coord
type Commands = [(Region, Command)]

type HouseLights = UArray Coord Int

-- Reference implementation against an immutable unboxed array
--
-- pt1 and pt2 follow the identical algorithm, and take about 700ms
--
-- This really hammers home how vastly superior as a starting point
-- arrays are for most things - this is slower than the optimized
-- RTree based pt1 (by quite a bit!), but is consistent over both
-- parts, and much shorter clearer code.
main :: IO ()
main = day 6 parser pt1 pt2 test
  where
    pt1 = print . luminosity . countLit
    pt2 = print . luminosity . getBrightness

houseBounds :: Region
houseBounds = (Coord 0 0, Coord 999 999)

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

unlitHouse :: HouseLights
unlitHouse = A.listArray houseBounds (repeat 0)

{-# INLINE lightHouse #-}
lightHouse :: (HouseLights -> Region -> Command -> [(Coord, Int)]) -> Commands -> HouseLights
lightHouse changes = L.foldl' go unlitHouse
  where
    go h (bs, cmd) = h // changes h bs cmd

countLit :: Commands -> HouseLights
countLit = lightHouse changes
  where
    changes _ bs Off    = [ (k, 0) | k <- A.range bs ]
    changes _ bs On     = [ (k, 1) | k <- A.range bs ]
    changes h bs Toggle = [ (k, 1 - (h ! k)) | k <- A.range bs ]

getBrightness :: Commands -> HouseLights
getBrightness = lightHouse changes
  where
    changes h bs cmd = [ (k, max 0 ((h ! k) + delta cmd)) | k <- A.range bs ]

    delta Off    = (-1)
    delta On     = 1
    delta Toggle = 2

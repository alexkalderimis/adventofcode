{-# LANGUAGE OverloadedStrings #-}

import           Control.Arrow           ((&&&))
import Control.Lens.Combinators (none)
import           Control.Monad           (guard)
import qualified Data.Array              as A
import           Data.Attoparsec.Text    (decimal, space)
import           Data.Ix
import qualified Data.List               as L
import           Data.List.NonEmpty      (NonEmpty)
import qualified Data.List.NonEmpty      as NE
import qualified Data.Set as S
import           Data.Maybe
import           Data.Monoid
import           Data.Ord
import qualified Data.Text               as T
import           Elves
import           Elves.Advent
import           Text.Parser.Char        (newline, text)
import           Text.Parser.Combinators (choice, sepBy1)

import           Test.QuickCheck         (Arbitrary (..), choose, listOf1,
                                          property, (===))

import           Elves.Cartesian
import           Elves.Tiling
import           Elves.Coord             (Bounds, overlaps, within)
import           Elves.RTree             (QueryStrategy (..), RTree, delete, query)
import qualified Elves.RTree             as RT

data Lit = On | Off deriving (Show, Eq)

data Command = Turn Lit | Toggle
  deriving (Show, Eq)

type Commands = [(Bounds Location, Command)]
type HouseLights = RTree Location Int
type LightRegion = (Bounds Location, Int)

main :: IO ()
main = day 6 parser pt1 pt2 test
  where
    pt1 = print . countLit
    pt2 = print . getBrightness

-- turn off 199,133 through 461,193
-- toggle 322,558 through 977,958
-- turn on 226,196 through 599,390
parser :: Parser Commands
parser = commandP `sepBy1` newline
  where
    location = (decimal <* ",") <#> decimal
    commandP = do
      cmd <- choice [Turn On <$ "turn on"
                    ,Turn Off <$ "turn off"
                    ,Toggle <$ "toggle"
                    ]
      space
      lb <- location
      text " through "
      ub <- location
      return ((lb,ub), cmd)

test = do

  describe "pt1" $ do
    let mr = parseOnly parser exampleInput
    it "evaluates correctly" $ do
      let expected = (1000 * 1000) - 1000 - 4
      fmap countLit mr `shouldBe` Right expected

  describe "pt2" $ do
    let mr = parseOnly parser exampleInput
    it "evaluates correctly" $ do
      let expected = (1000 * 1000) + 2000 - 4
      fmap getBrightness mr `shouldBe` Right expected

  describe "collisions" $ do
    let bs = ((1,1),(10,10))
        cs = [(bs, Toggle)
             ,(((5,5),(5,5)), Turn Off)
             ,(bs, Turn On)
             ,(bs, Toggle)
             ]
    describe "pt1" $ do
      let expected = 100 - 100
      it "calculates correctly" $ do
        countLit cs `shouldBe` expected
    describe "pt2" $ do
      let expected = (99 * (2 + 1 + 2)) + (1 * (2 - 1 + 1 + 2))
      it "calculates correctly" $ do
        getBrightness cs `shouldBe` expected

exampleInput = T.unlines
  ["turn on 0,0 through 999,999"
  ,"toggle 0,0 through 999,0"
  ,"turn off 499,499 through 500,500"
  ]

applyCommands :: (Command -> Int -> Int) -> Commands -> Int
applyCommands value
  = sum . fmap (\(bs, n) -> rangeSize bs * n) . RT.indexed . L.foldl' go mempty
  where
    go t (bs, cmd) = case query Overlapping bs t of
      [] -> RT.insert bs (value cmd 0) (t :: HouseLights)
      regions -> let (intersections, rest) = fmap concat
                                           . unzip
                                           . fmap (applyOverlay value bs cmd)
                                           $ (regions :: [LightRegion])

                     fillers = [(k, value cmd 0) | k <- tiles bs (fst <$> intersections)]
                  in L.foldl' (\t (k,v) -> RT.insert k v t)
                              (foldr delete t (fst <$> regions))
                              (intersections <> rest <> fillers)

applyOverlay :: (Command -> Int -> Int)
             -> Bounds Location -> Command -> LightRegion
             -> (LightRegion, [LightRegion])
applyOverlay value bs cmd (bs', n) =
  let intersection = trim bs bs'
      ns = neighbours bs' intersection
   in ((intersection, value cmd n), [(bs, n) | bs <- ns])

countLit :: Commands -> Int
countLit = applyCommands value
  where
    value Toggle 0     = 1
    value Toggle 1     = 0
    value (Turn On) _  = 1
    value (Turn Off) _ = 0

-- use an Endo Int to model the transformations along a path
-- throught the overlays.
getBrightness :: Commands -> Int
getBrightness = applyCommands value
  where
    value Toggle     n = n + 2
    value (Turn On)  n = n + 1
    value (Turn Off) n = max 0 (n - 1)

cmd :: (Bounds Location, Command) -> Command
cmd = snd

loc :: (Bounds Location, Command) -> Bounds Location
loc = fst

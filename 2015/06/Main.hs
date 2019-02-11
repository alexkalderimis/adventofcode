{-# LANGUAGE OverloadedStrings #-}

import           Control.Arrow           ((&&&))
import qualified Data.Array              as A
import           Data.Attoparsec.Text    (decimal, space)
import           Data.Ix
import qualified Data.List               as L
import qualified Data.List.NonEmpty               as NE
import Data.List.NonEmpty (NonEmpty)
import           Data.Maybe
import           Data.Monoid
import           Data.Ord
import qualified Data.Text               as T
import           Text.Parser.Char        (newline, text)
import           Text.Parser.Combinators (choice, sepBy1)

import           Test.QuickCheck         (Arbitrary (..), choose, property,
                                          (===))

import           Elves
import           Elves.Advent
import           Elves.Cartesian
import           Elves.Tiling
import           Elves.Coord (Bounds)
import           Elves.RTree             (QueryStrategy (..), RTree,
                                          delete, query)
import qualified Elves.RTree             as RT

type XPos = Int
type YPos = Int

data Lit = On | Off deriving (Show, Eq)

data Command = Turn Lit | Toggle
  deriving (Show, Eq)

type Commands = RTree Location (NonEmpty (Word, Command))

main :: IO ()
main = day 6 parser pt1 pt2 test
  where
    pt1 cs = print (countLit cs boardBounds)
    pt2 cs = print (getBrightness cs boardBounds)

-- turn off 199,133 through 461,193
-- toggle 322,558 through 977,958
-- turn on 226,196 through 599,390
parser :: Parser Commands
parser = commands <$> (commandP `sepBy1` newline)
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

commands :: [(Bounds Location, Command)] -> Commands
commands = RT.fromListWith (<>) . zipWith f [0 ..]
  where
    f prio (bs, cmd) = (bs, pure (prio, cmd))

test = do

  describe "pt1" $ do
    let mr = parseOnly parser exampleInput
    it "evaluates correctly" $ do
      let expected = (1000 * 1000) - 1000 - 4
      fmap (\cs -> countLit cs boardBounds) mr `shouldBe` Right expected

  describe "pt2" $ do
    let mr = parseOnly parser exampleInput
    it "evaluates correctly" $ do
      let expected = (1000 * 1000) + 2000 - 4
      fmap (\cs -> getBrightness cs boardBounds) mr `shouldBe` Right expected

  describe "collisions" $ do
    let bs = ((1,1),(10,10))
        cs = commands [(bs, Toggle)
                      ,(((5,5),(5,5)), Turn Off)
                      ,(bs, Turn On)
                      ,(bs, Toggle)
                      ]
    describe "pt1" $ do
      let expected = 100 - 100
      it "calculates correctly" $ do
        countLit cs bs `shouldBe` expected
    describe "pt2" $ do
      let expected = (99 * (2 + 1 + 2)) + (1 * (2 - 1 + 1 + 2))
      it "calculates correctly" $ do
        getBrightness cs bs `shouldBe` expected

exampleInput = T.unlines
  ["turn on 0,0 through 999,999"
  ,"toggle 0,0 through 999,0"
  ,"turn off 499,499 through 500,500"
  ]

boardBounds :: Bounds Location
boardBounds = ((0,0),(999,999))

countLit :: Commands -> Bounds Location -> Int
countLit cs bs = case topMost cs bs of
  Nothing -> 0
  Just ((bs', cmd), cs') ->
    let n = case cmd of Turn Off -> 0
                        Turn On  -> rangeSize bs'
                        Toggle   -> rangeSize bs' - countLit cs' bs'
    in n + sum (countLit cs' <$> neighbours bs bs')
  where
    topMost = popCommand Down

-- use an Endo Int to model the transformations along a path
-- throught the overlays.
getBrightness :: Commands -> Bounds Location -> Int
getBrightness = go (Endo id)
  where
    adjust a b = max 0 (a + b)
    bottomMost = popCommand id
    go endo cs bs = case bottomMost cs bs of
      Nothing -> fromIntegral (rangeSize bs) * appEndo endo 0
      Just ((bs', cmd), cs') ->
        let delta = case cmd of
                     Turn Off -> (-1)
                     Turn On  -> 1
                     Toggle   -> 2
            endo' = Endo (adjust delta) <> endo
            ns = neighbours bs bs'
         in sum (go endo' cs' bs' : fmap (go endo cs') ns)

-- get the next command, choosing by priority from multiple
-- candidate overlays
popCommand :: Ord a => (Word -> a) -> Commands -> Bounds Location
           -> Maybe ((Bounds Location, Command), Commands)
popCommand order cs bs = fmap (f . fmap snd) . listToMaybe $ overlaps
  where
    overlaps = L.sortBy (comparing (order . insertedAt . queryResult))
             . (>>= \(k, vs) -> (,) k <$> NE.toList vs)
             $ query Overlapping bs cs
    f found = (trimmed found, remove found cs)
    trimmed (bs', cmd) = (trim bs bs', cmd)
    remove (bs', _) = RT.alter (pure (popped bs')) bs'
    popped bs' = NE.nonEmpty
               . drop 1
               . fmap queryResult
               . L.sortBy (comparing (order . insertedAt . queryResult))
               . filter ((== bs') . fst)
               $ overlaps

-- Bunch of aliases for fst and snd to make life saner:

insertedAt :: (Word, Command) -> Word
insertedAt = fst

queryResult :: (Bounds Location, (Word, Command)) -> (Word, Command)
queryResult = snd

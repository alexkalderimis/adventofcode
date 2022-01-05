{-# LANGUAGE OverloadedStrings #-}

import           Control.Arrow           (second)
import           Data.Attoparsec.Text    (decimal, space)
import           Data.Ix                 (rangeSize)
import qualified Data.List               as L
import qualified Data.List.NonEmpty      as NE
import Data.List.NonEmpty (NonEmpty)
import           Data.Maybe
import           Data.Monoid
import           Data.Ord
import qualified Data.Text               as T
import           Data.Tree               (foldTree)
import           Text.Parser.Char        (newline, text)
import           Text.Parser.Combinators (choice, sepBy1)

import           Test.QuickCheck         (Arbitrary (..), choose, property,
                                          (===))

import           Elves
import           Elves.Advent
import           Elves.Cartesian         (Location)
import           Elves.Tiling            (trim, neighbours)
import           Elves.Coord             (Bounds)
import           Elves.RTree             (QueryStrategy (..), RTree, query)
import qualified Elves.RTree             as RT

type XPos = Int
type YPos = Int

data Lit = On | Off deriving (Show, Eq)

data Command = Turn Lit | Toggle
  deriving (Show, Eq)

type Priority = Word
type Commands = RTree Location (NonEmpty (Priority, Command))
type CommandList = [(Bounds Location, Command)]

-- an optimized approach to the problem.
--
-- For pt1, we use the insight that the the commands are LIFO with
-- regards to outcome - a light switched on or off at the end stays
-- on or off, regardless of what happened to it previously. So we
-- go through the commands in reverse, and when we find a command
-- that is final, we can ignore all commands the overlap with it.
-- pt1 takes about 200ms
--
-- For pt2 we cannot ignore any commands, so have to go through all of them.
-- We go through them from the bottom up (from the first applied command to
-- the last), and represent each area with an (Endo Int) modelling the
-- transformations applied to it.
-- pt2 takes about 65s
main :: IO ()
main = day 6 parser pt1 pt2 test
  where
    pt1 = print . countLit . commands
    pt2 = print . getBrightness . commands

-- turn off 199,133 through 461,193
-- toggle 322,558 through 977,958
-- turn on 226,196 through 599,390
parser :: Parser CommandList
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

commands :: CommandList -> Commands
commands = fmap orderCommands . RT.fromListWith (<>) . zipWith prioritize [0 ..]
  where
    prioritize prio (bs, cmd) = (bs, [(prio, cmd)])
    orderCommands = NE.fromList
                  . L.sortBy (comparing insertedAt)

test = do
  describe "exampleInput" $ do
    let Right cs = commands <$> parseOnly parser exampleInput

    describe "pt1" $ do
      it "evaluates correctly" $ do
        let expected = (1000 * 1000) - 1000 - 4
        countLit cs `shouldBe` expected

    describe "pt2" $ do
      it "evaluates correctly" $ do
        let expected = (1000 * 1000) + 2000 - 4
        getBrightness cs `shouldBe` expected

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

countLit :: Commands -> Int
countLit cs = go (NE.reverse <$> cs) (RT.bounds' cs)
  where
    go t bs = case topMost t bs of
                Nothing -> 0
                Just ((bs', cmd), t') ->
                  let n = case cmd of Turn Off -> 0
                                      Turn On  -> rangeSize bs'
                                      Toggle   -> rangeSize bs' - go t' bs'
                      ns = neighbours bs bs'
                  in sum (n : fmap (go t') ns)

-- use an Endo Int to model the transformations along a path
-- throught the overlays.
getBrightness :: Commands -> Int
getBrightness t = go (Endo id) t (RT.bounds' t)
  where
    adjust a b = max 0 (a + b)

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

-- take the highest priority command (LIFO)
topMost :: Commands -> Bounds Location -> Maybe ((Bounds Location, Command), Commands)
topMost = popCommand Down

-- take the lowest priority command (FIFO)
bottomMost :: Commands -> Bounds Location -> Maybe ((Bounds Location, Command), Commands)
bottomMost = popCommand id

-- pop the next command overlapping the search box, choosing by priority from multiple
-- candidate overlays.
--
-- The resulting command is returned with its bounds trimmed to the intersection
-- of the search box and the command box.
popCommand :: (Ord a) => (Priority -> a) -> Commands -> Bounds Location -> Maybe ((Bounds Location, Command), Commands)
popCommand order cs bs = f . forgetPrio <$> highestPriority overlaps
  where
    forgetPrio      = second command
    highestPriority = listToMaybe . L.sortOn (order . qrPriority)
    overlaps           = query Overlapping bs cs >>= \(k, vs) -> [(k, NE.head vs)]
    f found            = (trimmed found, RT.alter pop (location found) cs)
    trimmed (bs', cmd) = (trim bs bs', cmd)
    pop                = (>>= NE.nonEmpty . NE.tail)

-- Bunch of aliases for fst and snd to make life saner:

insertedAt :: (Priority, Command) -> Priority
insertedAt = fst

command :: (Priority, Command) -> Command
command = snd

location :: (Bounds Location, Command) -> Bounds Location
location = fst

type QueryResult = (Bounds Location, (Priority, Command))

qrPriority :: QueryResult -> Priority
qrPriority = insertedAt . snd

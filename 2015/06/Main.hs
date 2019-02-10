{-# LANGUAGE OverloadedStrings #-}

import           Control.Arrow           ((&&&))
import qualified Data.Array              as A
import           Data.Attoparsec.Text    (decimal, space)
import           Data.Ix
import qualified Data.List               as L
import           Data.Maybe
import           Data.Monoid
import           Data.Ord
import qualified Data.Text               as T
import           Elves
import           Elves.Advent
import           Text.Parser.Char        (newline, text)
import           Text.Parser.Combinators (choice, sepBy1)

import           Test.QuickCheck         (Arbitrary (..), choose, property,
                                          (===))

import           Elves.Cartesian
import           Elves.RTree             (Bounds, QueryStrategy (..), RTree,
                                          delete, query)
import qualified Elves.RTree             as RT

type XPos = Int
type YPos = Int

data Lit = On | Off deriving (Show, Eq)

data Command = Turn Lit | Toggle
  deriving (Show, Eq)

type Commands = RTree Location (Word, Command)

data NeighbourInput = NeighbourInput (Bounds Location) (Bounds Location)
  deriving (Show, Eq)

instance Arbitrary NeighbourInput where
  arbitrary = do
    tly <- choose (0, 1000)
    tlx <- choose (0, 1000)
    bly <- choose (tly, 1000)
    blx <- choose (tlx, 1000)

    tly' <- choose (tly, bly)
    tlx' <- choose (tlx, blx)
    bly' <- choose (tly', bly)
    blx' <- choose (tlx', blx)

    return (NeighbourInput ((tly,tlx),(bly,blx)) ((tly',tlx'),(bly',blx')))

main :: IO ()
main = day 6 parser pt1 pt2 test
  where
    pt1 cs = print (countLit cs boardBounds)
    pt2 cs = print (getBrightness cs boardBounds)

-- turn off 199,133 through 461,193
-- toggle 322,558 through 977,958
-- turn on 226,196 through 599,390
parser :: Parser Commands
parser = fmap (RT.fromList . zipWith f [0 ..]) (commandP `sepBy1` newline)
  where
    f prio (bs, cmd) = (bs, (prio, cmd))
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
      fmap (\cs -> countLit cs boardBounds) mr `shouldBe` Right expected

  describe "pt2" $ do
    let mr = parseOnly parser exampleInput
    it "evaluates correctly" $ do
      let expected = (1000 * 1000) + 2000 - 4
      fmap (\cs -> getBrightness cs boardBounds) mr `shouldBe` Right expected

  describe "neighbours" $ do
    describe "the eight 3x3 boxes around a 3x3 box" $ do
      let ns = neighbours ((0,0), (8,8)) ((3,3), (5,5))
      it "all have 9 locations" $ do
        fmap rangeSize ns `shouldSatisfy` all (== 9)
    specify "there are at most 8 neighbours" $ property $ \(NeighbourInput outer inner) ->
      let ns = neighbours outer inner
       in length ns `elem` [0 .. 8]
    specify "no neighbour is bigger than the outer region" $ property $ \(NeighbourInput outer inner) ->
      let ns = neighbours outer inner
       in rangeSize outer > maximum (rangeSize <$> ns)
    it "accounts for all the locations" $ property $ \(NeighbourInput outer inner) ->
      let ns = neighbours outer inner
       in rangeSize outer === sum (rangeSize <$> inner : ns)

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
popCommand order cs bs
           = fmap (f . fmap snd)
           . listToMaybe
           . L.sortBy (comparing (order . insertedAt . queryResult))
           $ query Overlapping bs cs
  where
    f (bs', cmd) = ((trim bs bs', cmd), delete bs' cs)

-- trim two overlapping bounds to their intersection
trim :: Bounds Location -> Bounds Location -> Bounds Location
trim a b = ((max (topEdge a) (topEdge b), max (leftEdge a) (leftEdge b))
           ,(min (btmEdge a) (btmEdge b), min (rightEdge a) (rightEdge b))
           )

-- returns the bounds of the eight neighbours: TL, L, BL, T, B, TR, R, BR
--  
--   outer
--  a--------b-----------c--------d  -
--  | TL     |    T      |  TR    |  | Top-margin
--  |       f|           g        |  |
--  e--------+-----------+g'------h  -
--  | L      |inner      |  R     |  | Inner-height
--  |        |           |        |  |
--  i-------j+-----------k--------l  -
--  | BL     j'   B      |  BR    |  | Bottom-margin
--  |        |           |        |  |
--  m-------n-----------o---------p  -
--
--  |--------|-----------|--------|
--    Left     Inner       Right
--    margin   width       margin
--
neighbours :: Bounds Location -> Bounds Location -> [Bounds Location]
neighbours outer inner = concat [tl, l_, bl, t_, b_, tr, r_, br]
  where
    tl = [ (a, f)  | leftMarginExists && topMarginExists ]
    l_ = [ (e, j)  | leftMarginExists ]
    bl = [ (i, n)  | leftMarginExists && bottomMarginExists ]
    t_ = [ (b, g)  | topMarginExists ]
    b_ = [ (j', o) | bottomMarginExists ]
    tr = [ (c, h)  | topMarginExists && rightMarginExists ]
    r_ = [ (g', l) | rightMarginExists ]
    br = [ (k, p)  | bottomMarginExists && rightMarginExists ]

    leftMarginExists   = leftEdge outer  < leftEdge inner
    topMarginExists    = topEdge outer   < topEdge inner
    rightMarginExists  = rightEdge inner < rightEdge outer
    bottomMarginExists = btmEdge inner   < btmEdge outer

    -- minor adjustments are made to exclude inner
    a = topLeft outer
    b = move North topMargin (topLeft inner)
    c = right $ move North topMargin (topRight inner)
    d = topRight outer
    e = move South topMargin a
    f = up . left $ topLeft inner
    g = up $ topRight inner
    g' = right $ topRight inner
    h = up $ move East rightMargin (topRight inner)
    i = move South (innerHeight + 1) e
    j = left $ btmLeft inner
    j' = down $ btmLeft inner
    k = move South (innerHeight + 1) g'
    l = move East rightMargin (btmRight inner)
    m = btmLeft outer
    n = left $ move South bottomMargin (btmLeft inner)
    o = move South bottomMargin (btmRight inner)
    p = btmRight outer

    leftMargin = leftEdge inner - leftEdge outer
    topMargin  = topEdge inner - topEdge outer
    bottomMargin = btmEdge outer - btmEdge inner
    rightMargin = rightEdge outer - rightEdge inner
    innerHeight = btmEdge inner - topEdge inner

-- Bunch of aliases for fst and snd to make life saner:

insertedAt :: (Word, Command) -> Word
insertedAt = fst

queryResult :: (Bounds Location, (Word, Command)) -> (Word, Command)
queryResult = snd

leftEdge :: Bounds Location -> XPos
leftEdge = x . lb

rightEdge :: Bounds Location -> XPos
rightEdge = x . ub

topEdge :: Bounds Location -> YPos
topEdge = y . lb

btmEdge :: Bounds Location -> YPos
btmEdge = y . ub

topLeft :: Bounds Location -> Location
topLeft = topEdge &&& leftEdge

topRight :: Bounds Location -> Location
topRight = topEdge &&& rightEdge

btmRight :: Bounds Location -> Location
btmRight = btmEdge &&& rightEdge

btmLeft :: Bounds Location -> Location
btmLeft = btmEdge &&& leftEdge

lb :: Bounds Location -> Location
lb = fst

ub :: Bounds Location -> Location
ub = snd

x :: Location -> Int
x = snd

y :: Location -> Int
y = fst


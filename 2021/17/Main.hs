{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

import Data.Maybe (catMaybes)
import Data.Ord (Down)
import qualified Data.List as L
import qualified Data.Text as T
import           Data.Text (Text)
import qualified Data.Attoparsec.Text as A
import Data.Coerce (coerce)
import qualified Data.Array as Array
import           Data.Array (Array, (//))
import           Control.Applicative.Combinators
import           Data.Attoparsec.Text            (signed, decimal)
import           Text.Parser.Char (space, text)
import qualified Data.Set as Set
import           Data.Set (Set)

import Elves
import Elves.Advent
import Elves.StrictGrid
import Elves.Coord (origin)

-- for the purposes of this puzzle, we know that the target is **always** below the starting position.
-- we do not need to solve for the general case.
--
-- So solutions will look something like:
--
--  λ> let target = (Coord (Row (-10)) (Col 10), Coord (Row (-5)) (Col 30))
--  λ> let start = origin
--  λ> putStrLn $ illustrate start target (Delta { dx = 6, dy = 3 })
--  ...............#..#............ -+
--  ...........#........#..........  |
--  ...............................  |
--  ......#..............#.........  |- initial parabolic arc
--  ...............................  |
--  ............................... -+
--  S====================#========= --- inversion, DV == -DV
--  ............................... -+
--  ...............................  |
--  ...............................  |
--  .....................#.........  |
--  ..........TTTTTTTTTTTTTTTTTTTTT  |- Descent
--  ..........TTTTTTTTTTTTTTTTTTTTT  |
--  ..........TTTTTTTTTTTTTTTTTTTTT  |
--  ..........TTTTTTTTTTTTTTTTTTTTT  |
--  ..........TTTTTTTTTTT#TTTTTTTTT  |= Hit
--  ..........TTTTTTTTTTTTTTTTTTTTT -+ (baseline)
--
-- For each X position in the target, we can determine which
-- DX values can result in a hit. We need to distinguish between
-- hits where DX == 0 (lobs) and ones where DX > 0 at some T.
--
-- If DX is positive, then we can calculate the intial DV given the
-- step number (T). There can only be one value.
--
-- If the target can be hit with a vertically descending lob
-- (i.e. DX can be chosen such that DX falls to 0 over the
-- target), then we must consider infinitely many T points (since
-- DX will remain at 0 for all future T). We limit this search by setting
-- an upper-bound on DV.
--
-- If `baseline <= -DV`, then the target cannot be hit
-- using a vertically descending lob, because DV will be (-DV - 1)
-- as soon as it passes the inversion. e.g:
-- 
--  λ> putStrLn $ illustrate start target (Delta { dx = 6, dy = 10 })
--  .....................#.........
--  .....................#.........
--  ...............................
--  .....................#.........
--  ...............................
--  ...............................
--  .....................#.........
--  ...............................
--  ...............................
--  ...............................
--  .....................#......... -+
--  ...............................  |
--  ...............................  |
--  ...............................  |
--  ...............................  |
--  ....................##......... -+
--  ...............................  |
--  ...............................  |
--  ...............................  |- note the paired ascent and descent velocities
--  ...............................  |
--  ...............................  |
--  ..................#..#......... -+
--  ...............................  |
--  ...............................  |
--  ...............................  |
--  ...............................  |
--  ...............................  |
--  ...............................  |
--  ...............#.....#......... -+
--  ...............................
--  ...............................
--  ...............................
--  ...............................
--  ...............................
--  ...............................
--  ...............................
--  ...........#.........#.........
--  ...............................
--  ...............................
--  ...............................
--  ...............................
--  ...............................
--  ...............................
--  ...............................
--  ...............................
--  ......#..............#.........
--  ...............................
--  ...............................
--  ...............................
--  ...............................
--  ...............................
--  ...............................
--  ...............................
--  ...............................
--  ...............................
--  S====================#========= -+
--  ...............................  |
--  ...............................  |
--  ...............................  |
--  ...............................  |
--  ..........TTTTTTTTTTTTTTTTTTTTT  |
--  ..........TTTTTTTTTTTTTTTTTTTTT  |- DV = -(DV + 1)
--  ..........TTTTTTTTTTTTTTTTTTTTT  |
--  ..........TTTTTTTTTTTTTTTTTTTTT  |
--  ..........TTTTTTTTTTTTTTTTTTTTT  |
--  ..........TTTTTTTTTTTTTTTTTTTTT  |
--  .....................#......... -+
--
--  This puts an upper limit on DV for straight lobs of (baseline - 1).

type Coord = Coordinate
type Target = (Coord, Coord)
newtype Steps    = T Int deriving (Show, Eq, Ord, Num, Enum)
newtype Velocity = V Int deriving (Show, Eq, Ord, Num)
newtype Distance = D Int deriving (Show, Eq, Ord, Num, Enum)

-- given a start and a goal, what number of steps do I need to hit it, at a given velocity?
validVX :: Coord -> Coord -> [(Steps, Velocity)]
validVX start goal = [minVX .. maxVX] >>= steps . V
  where
    distance = getCol (col goal) - getCol (col start)
    minVX = ceiling $ sqrt (fromIntegral distance :: Double)
    maxVX = distance
    steps vx = fmap (fmap (const vx)) $ filter ((== D distance) . snd) (xCovered vx)

isLob :: (Steps, Velocity) -> Bool
isLob (t, v) = dxAt t v == 0

maxLobDV :: Target -> Velocity
maxLobDV target = let baseline = min (row (fst target)) (row (snd target))
                   in V (negate (coerce $ baseline) - 1)

maxMovingShotDV :: [(Distance, Steps)] -> Velocity
maxMovingShotDV = head . catMaybes . fmap (uncurry yvelocity) . L.sortOn (Down . snd)

xCovered :: Velocity -> [(Steps, Distance)]
xCovered (V vx) = zip [0..] . fmap D . L.scanl (+) 0 $ filter (> 0) [vx, vx - 1 .. 0]

ypos :: Steps -> Velocity -> Distance
ypos (T t) (V v) = D (v * t) - accelDistance (T t)

-- dx = x0 + (v0 * t) + accelDistance(t)
-- ======
-- or: dx = (v0 * t) - accelDistance(t), since vertical acceleration is always negative
-- dx + accelDistance(t) = v * t
-- dx + accelDistance / t = v
yvelocity :: Distance -> Steps -> Maybe Velocity
yvelocity d t = V <$> (coerce (d + accelDistance t) `exactDiv` coerce t)

exactDiv :: Int -> Int -> Maybe Int
exactDiv _ 0 = Nothing
exactDiv a b = let c = div a b in if c * b == a then Just c else Nothing

-- 0.5 a t^2, but a is (-1), so can be omitted.
accelDistance :: Steps -> Distance
accelDistance (T n) = D ((n * pred n) `div` 2)

dxAt :: Steps -> Velocity -> Velocity
dxAt (T n) (V dx) = V (atLeast 0 $ dx - n)

highestInitialDV :: Coord -> Target -> Velocity
highestInitialDV start target
  = let vxs = [(goal, solution) | goal <- Array.range target, solution <- validVX start goal]
        (lobs, movingHits) = L.partition (isLob . snd) vxs
     in if null lobs
        then maxMovingShotDV [(coerce (row g) - coerce (row start), t) | (g, (t, _)) <- movingHits]
        else maxLobDV target

maxHeightReached :: Coord -> Velocity -> Distance
maxHeightReached start v | v <= 0 = coerce (row start)
maxHeightReached start v = L.foldl' (+) (coerce $ row start) [0 .. coerce v]

allPossibleSolutions :: Coord -> Target -> Set Delta
allPossibleSolutions start target
  = let vxs = [(goal, solution) | goal <- Array.range target, solution <- validVX start goal]
     in Set.fromList (vxs >>= solve start target)

solve :: Coord -> Target -> (Coord, (Steps, Velocity)) -> [Delta]
solve start target (goal, (t, dx))
  | isLob (t, dx) = let min_t = t
                        parabola_len = 2 * (coerce (maxLobDV target) + 1)
                        max_t = parabola_len + 1
                        dvs = catMaybes [yvelocity d t | t <- [min_t .. max_t]]
                     in [Delta { dx = coerce dx, dy = coerce dy } | dy <- dvs ]
  | otherwise = case yvelocity d t of
                      Just dy -> [Delta { dx = coerce dx, dy = coerce dy }]
                      Nothing -> []
  where
    d = coerce (row goal) - coerce (row start)

-- useful for the general case, but not for this puzzle.
-- goalBelowStart :: Coord -> Target -> Bool
-- goalBelowStart start t = row start > row (fst t)

-- Draw a little diagram, as in the instructions
illustrate :: Coord -> Target -> Delta -> String
illustrate start target d = do
  let (path, rst) = L.span ((>= baseLine) . row) $ positions start d
      allPoints = start : fst target : snd target : head rst : path 
      Just (minX, maxX) = minmax (col <$> allPoints)
      Just (minY, maxY) = minmax (row <$> allPoints)
      bs = (Coord minY minX, Coord maxY maxX)
      grid = Array.listArray bs (repeat '.')
             // [(i, '=') | i <- Array.range (Coord (row start) minX, Coord (row start) maxX)]
             // [(i, 'T') | i <- Array.range target]
             // [(i, '#') | i <- (head rst : path)]
             // [(start, 'S')]
   in unlines . reverse . lines $ draw grid
  where
    baseLine = row (fst target)

positions :: Coord -> Delta -> [Coord]
positions = let f (p, v) = (move p v, accelerate dv v)
                dv = Accel { ddy = pred, ddx = atLeast 0 . pred }
             in curry (fmap fst . iterate f)

main :: IO ()
main = day 17 parser pt1 pt2 test
  where
    pt1 target = case maxHeightReached origin (highestInitialDV origin target) of
                   D n -> print n
    pt2 = print . Set.size . allPossibleSolutions origin

test = do
  let exampleInput = "target area: x=20..30, y=-10..-5"
  let parsed = parseOnly parser exampleInput

  it "parses correctly" $ do
    parsed `shouldBe` Right (Coord (Row (-10)) (Col 20), Coord (Row (-5)) (Col 30))

  before (either fail pure parsed) $ do

    it "finds the mighest initial vertical velocity" $ \target -> do
      highestInitialDV origin target `shouldBe` V 9

    it "finds the correct solution to pt1" $ \target -> do
      maxHeightReached origin (highestInitialDV origin target) `shouldBe` D 45

    context "all possible solutions" $ do
      let solutions = [ "23,-10  25,-9   27,-5   29,-6   22,-6   21,-7   9,0     27,-7   24,-5"
                      , "25,-7   26,-6   25,-5   6,8     11,-2   20,-5   29,-10  6,3     28,-7"
                      , "8,0     30,-6   29,-8   20,-10  6,7     6,4     6,1     14,-4   21,-6"
                      , "26,-10  7,-1    7,7     8,-1    21,-9   6,2     20,-7   30,-10  14,-3"
                      , "20,-8   13,-2   7,3     28,-8   29,-9   15,-3   22,-5   26,-8   25,-8"
                      , "25,-6   15,-4   9,-2    15,-2   12,-2   28,-9   12,-3   24,-6   23,-7"
                      , "25,-10  7,8     11,-3   26,-7   7,1     23,-9   6,0     22,-10  27,-6"
                      , "8,1     22,-8   13,-4   7,6     28,-6   11,-4   12,-4   26,-9   7,4"
                      , "24,-10  23,-8   30,-8   7,0     9,-1    10,-1   26,-5   22,-9   6,5"
                      , "7,5     23,-6   28,-10  10,-2   11,-1   20,-9   14,-2   29,-7   13,-3"
                      , "23,-5   24,-8   27,-9   30,-7   28,-5   21,-10  7,9     6,6     21,-5"
                      , "27,-10  7,2     30,-9   21,-8   22,-7   24,-9   20,-6   6,9     29,-5"
                      , "8,-2    27,-8   30,-5   24,-7"
                      ]
          vectorP = flip sepBy1 (some space) (flip Delta <$> signed decimal <*> (text "," *> signed decimal))
          Right vectors = fmap Set.fromList $ parseOnly vectorP (T.intercalate " " solutions)

      it "finds all possible solutions" $ \target -> do
        allPossibleSolutions origin target `shouldBe` vectors

      it "finds all possible solutions at dx=6" $ \target -> do
        let dx_6 = Set.filter $ \d -> dx d == 6
        dx_6 (allPossibleSolutions origin target) `shouldBe` dx_6 vectors

parser :: Parser Target
parser = do
  text "target area: "
  text "x="
  minX <- Col <$> signed decimal
  text ".."
  maxX <- Col <$> signed decimal
  text ", "
  text "y="
  minY <- Row <$> signed decimal
  text ".."
  maxY <- Row <$> signed decimal
  pure (Coord minY minX, Coord maxY maxX)

import qualified Control.Concurrent.Async as Async
import Control.Exception (evaluate)
import Control.Concurrent.Async (async)
import Control.Monad
import Data.Int
import Data.List (mapAccumL, maximumBy)
import Data.Ord
import System.Environment (withArgs, getArgs)
import System.Exit
import Test.QuickCheck (property)
import Text.Printf
import Text.Read (readMaybe)
import qualified Data.Array.Unboxed as A
import           Data.Attoparsec.Text (decimal)

import Elves.Advent

newtype SerialNo = SerialNo { unSO :: Int } deriving (Show)
type FuelLevel = Int
type Coord = (Int, Int)
type FuelCells = A.UArray Coord FuelLevel

main :: IO ()
main = day 11 parser pt1 pt2 spec
  where
    parser = initFuelCell . SerialNo <$> decimal
    pt1 = print . selectMaxSquare 3
    pt2 = printPt2 . partTwo
    printPt2 (size, ((x,y), total)) =
      printf "Total %d: %d,%d,%d\n" total x y size

fuelCellBounds :: (Coord, Coord)
fuelCellBounds = ((1,1), (300,300))

translate :: Coord -> Coord -> Coord
translate (dx,dy) (x,y) = (dx + x, dy + y)

type Size = Int
type Total = Int

-- doing the naive thing (max [selectMaxSquare i cells | i <- [1 .. 300]]) is
-- extremely expensive, taking over 10 minutes to complete. This solution
-- completes in about 11sec, using dynamic programming.
--
-- The key optimisation is recognising that the sums of smaller stages can be
-- reused for the larger sizes. So as we proceed, we memoize the previous
-- steps. E.G:
--
-- ###.
-- ###.
-- ###.
-- ....
--
-- If we already know the sum of the #'s at origin (x,y) (size = 3), then the
-- sum of the square at the same origin but at size 4 is the sum of the #'s and
-- the .'s. If we record the (pos -> sum) mapping for each stage, then we can
-- just look up the previous value and add the missing edges to it. So in this
-- case we save 9 lookups, or 56%. At larger sizes this really pays off. At size
-- 30, we would save 93% of the lookups, and over 99% by the 200th state. Just
-- as we avoid the calculation with a lookup table, we avoid almost all the
-- array reads as we progress by forming squares onion-wise from their smaller
-- forebears. At each stage we only have one memoised array around (that from
-- the previous state), in addition to the main fuel-cells array.
partTwo :: FuelCells -> (Size, (Coord, Total))
partTwo cells = maximumBy (comparing (snd.snd))
              . snd
              $ mapAccumL go cells [1 .. 300]
  where
    go a 1 = (a, ret 1 cells) -- no translation/derivation required
    go a n = let bds = translate (-1,-1) <$> A.bounds a
                 a'  = A.array bds $ fmap (\ix -> (ix, oneBigger n a ix)) (A.range bds)
              in (a', ret n a')

    ret n a           = (n, maxAssoc a)
    rhEdge os origin  = (translate (os, 0) origin, translate (os    , os) origin)
    btmEdge os origin = (translate (0, os) origin, translate (os - 1, os) origin)
    oneBigger size a coord = let offset = size - 1
                              in sum [ (a :: FuelCells) A.! coord
                                     , gridTotal cells (rhEdge offset coord)
                                     , gridTotal cells (btmEdge offset coord)
                                     ]

maxAssoc :: FuelCells -> (Coord, FuelLevel)
maxAssoc = maximumBy (comparing snd) . A.assocs 

selectMaxSquare :: Int -> FuelCells -> (Coord, Int)
selectMaxSquare 1 cells = maxAssoc cells
selectMaxSquare size cells = maximumBy (comparing snd) $ do
  let offset = size - 1
  pos <- A.range . fmap (translate (negate offset, negate offset))
                 $ fuelCellBounds
  let bds = (pos, translate (offset,offset) pos)
  return (pos, gridTotal cells bds)

gridTotal :: FuelCells -> (Coord, Coord) -> Int
gridTotal cells = sum . fmap (cells A.!) . A.range

subGridAt :: (Coord, Coord) -> FuelCells -> FuelCells
subGridAt bds cells = A.array bds
  [(pos, cells A.! pos) | pos <- A.range bds]

initFuelCell :: SerialNo -> FuelCells
initFuelCell sn = A.array fuelCellBounds
  [(pos, currentCellLevel sn pos) | pos <- A.range fuelCellBounds]

currentCellLevel :: SerialNo -> Coord -> FuelLevel
currentCellLevel (SerialNo serialNo) (x,y) =
  toEnum $ hundreds (rackId * (serialNo + (rackId * y))) - 5
  where
    rackId = x + 10

hundreds :: Int -> Int
hundreds n = rem (n `div` 100) 10

spec :: Spec
spec = do
  describe "currentCellLevel" $ do
    let examples = [(SerialNo 8,  (3,5), 4)
                   ,(SerialNo 57, (122, 79), -5)
                   ,(SerialNo 39, (217, 196), 0)
                   ,(SerialNo 71, (101, 153), 4)
                   ]
    forM_ examples $ \(sn, pos, expected) -> do
      describe (printf "at (%d,%d) in grid %d"
                           (fst pos) (snd pos)
                           (unSO sn)) $ do
        it (printf "the power level is %d" expected) $ do
          currentCellLevel sn pos `shouldBe` expected
  describe "partTwo" $ do
    it "can find the max square for (SerialNo 18)" $ do
      partTwo (initFuelCell (SerialNo 18))
        `shouldBe` (16, ((90,269),113))
    it "can find the max square for (SerialNo 42)" $ do
      partTwo (initFuelCell (SerialNo 42))
        `shouldBe` (12, ((232,251),119))
  describe "selectMaxSquare" $ do
    it "can find the max 3x3 square for (SerialNo 18)" $ do
      selectMaxSquare 3 (initFuelCell (SerialNo 18))
        `shouldBe` ((33,45), 29)
    it "can find the max 16x16 square for (SerialNo 18)" $ do
      selectMaxSquare 16 (initFuelCell (SerialNo 18))
        `shouldBe` ((90, 269), 113)
    it "cannot find a bigger square than that optimum 16x16 square" $ do
      selectMaxSquare 25 (initFuelCell (SerialNo 18))
        `shouldSatisfy` (\(_, val) -> val < 113)
    it "can find the max 3x3 square for (SerialNo 42)" $ do
      selectMaxSquare 3 (initFuelCell (SerialNo 42))
        `shouldBe` ((21,61), 30)
  describe "hundreds" $ do
    it "returns the hundreds digit, as an int" $ do
      property $ \i -> hundreds (abs i) `elem` [0 .. 9]
    it "returns the correct 100s digit" $ do
      property $ \i ->
        let posi = abs i
         in posi == (1000 * (posi `div` 1000)
                     + 100 * hundreds posi
                     + rem posi 100)

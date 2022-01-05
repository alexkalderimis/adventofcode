module Elves.TilingSpec where

import Data.Ix
import Data.Ord
import qualified Data.List as L
import Control.Lens.Combinators (none)
import qualified Data.Map as M

import Test.Hspec
import Test.QuickCheck hiding (scale, NonZero, within)

import Elves.Coord hiding (corners)
import Elves.Cartesian
import Elves.Tiling
import qualified Elves.CountMap as CM

import Support.TilingSupport (splitOf, xSplitOf, ySplitOf, NeighbourInput(..), TileInput(..))
import Support.BoundingBoxes (Region(..))

spec :: Spec
spec = describe "Elves.Tiling" $ do
  tilesSpec
  neighboursSpec
  compactifySpec
  splitsSpec

splitsSpec = describe "splitting" $ do
  let r = ((-100, -100), (100, 100))
      small = ((-10, -10), (10, 10))

  --  +-----------+
  --  |--------|--|
  --  |   |  | |  |
  --  |---|--|-|--|
  --  |   |  | |  |
  --  |------|-|--|
  --  |   |  | |  |
  --  +-----------+
  --  0 = 1
  --  1 = 2
  --  2 = 4
  --  3 = 6
  --  4 = 9
  --  5 = 12
  --  6 = 16
  it "increases the number of regions by this sequence, if splits alternate" $
    forAll (ySplitOf r)                    $ \s0 ->
    forAll (xSplitOf r)                    $ \s1 ->
    forAll (ySplitOf r `suchThat` (/= s0)) $ \s2 ->
    forAll (xSplitOf r `suchThat` (/= s1)) $ \s3 -> do
      let divisions = [s0, s1, s2, s3]
      let rs = splits r divisions
      length rs `shouldBe` 9

  it "adds a new item each time, if splits are in the same direction, Y" $
    forAll (L.nub <$> listOf (ySplitOf r)) $ \divisions -> do
      let rs = splits r divisions
      length rs `shouldBe` 1 + length divisions

  it "adds a new item each time, if splits are in the same direction, X" $
    forAll (L.nub <$> listOf (xSplitOf r)) $ \divisions -> do
      let rs = splits r divisions
      length rs `shouldBe` 1 + length divisions

  -- use the smaller region for speed
  it "does not change coverage" $
    forAll (L.nub <$> vectorOf 5 (splitOf small)) $ \divisions -> do
      let rs = splits small divisions
      CM.fromList (rs >>= range) `shouldBe` CM.fromList (range small)

  it "splits along the Y axis" $
    forAll (ySplitOf r) $ \s -> do
      let rs = splitRegion s r
      L.nub (leftEdge <$> rs) `shouldBe` [leftEdge r]
      L.nub (rightEdge <$> rs) `shouldBe` [rightEdge r]

  it "splits along the X axis" $
    forAll (xSplitOf r) $ \s -> do
      let rs = splitRegion s r
      L.nub (topEdge <$> rs) `shouldBe` [topEdge r]
      L.nub (btmEdge <$> rs) `shouldBe` [btmEdge r]

compactifySpec = describe "compactify" $ do
  describe "recursively splitting a region and then compacting it" $ do
    let r = ((0,0), (100, 100))
        divisions = [Left 50, Right 25, Left 70, Right 10]
        rs = splits r divisions
    it "restores the original region" $ do
      compactify rs `shouldBe` [r]

  describe "compactify of neighbours" $ do

    it "can produce at most 4 regions" $ do
      let outer = ((0,0), (100,100))
          inner = ((20,20), (80,80))
          ns = neighbours outer inner
      length (compactify ns) `shouldBe` 4

    it "produces regions that are identical in coverage to the input" $ do
      property $ \(NeighbourInput outer inner) -> do
        let ns = neighbours outer inner
            ns' = compactify ns
        (ns >>= range) `shouldMatchList` (ns' >>= range)

tilesSpec = describe "tiles" $ do

    -- verify the input itself
    -- these are the preconditions to tiles
    describe "TileInput" $ do

      specify "all intersections are entirely within outer" 
        $ property $ \(TileInput outer is) ->
           all (\x -> contains x outer) is

      specify "all intersections are different"
        $ property $ \(TileInput _ is) -> L.nub is === is

      specify "no intersection overlaps any other"
        $ property $ \(TileInput _ is) ->
          none (\x -> any (overlaps x) (filter (/= x) is)) is

    describe "manually worked out example" $ do
      let outer = ((0,0),(26,24))
          is = [((0,0),(4,6))
               ,((3,18),(10,24))
               ,((12,0),(20,7))
               ,((16,12),(22,18))
               ]

      --  // = intersection
      --  .. = tile
      --  ┌─────┐┌┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┐
      --  │/////│┊................┊
      --  │/////│└┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┘
      --  │/////│┌┈┈┈┈┈┈┈┈┈┐┌─────┐
      --  └─────┘┊.........┊│/////│
      --  ┌┈┈┈┈┈┐┊.........┊│/////│
      --  ┊.....┊┊.........┊│/////│
      --  ┊.....┊┊.........┊│/////│
      --  ┊.....┊┊.........┊│/////│
      --  ┊.....┊┊.........┊│/////│
      --  ┊.....┊┊.........┊└─────┘
      --  └┈┈┈┈┈┘└┈┈┈┈┈┈┈┈┈┘┌┈┈┈┈┈┐
      --  ┌──────┐┌┈┈┈┈┈┈┈┈┐┊.....┊
      --  │//////│┊........┊┊.....┊
      --  │//////│┊........┊┊.....┊
      --  │//////│└┈┈┈┈┈┈┈┈┘└┈┈┈┈┈┘
      --  │//////│┌┈┈┐┌─────┐┌┈┈┈┈┐
      --  │//////│┊..┊│/////│┊....┊
      --  │//////│┊..┊│/////│┊....┊
      --  │//////│┊..┊│/////│┊....┊
      --  └──────┘┊..┊│/////│┊....┊
      --  ┌┈┈┈┈┈┈┐┊..┊│/////│┊....┊
      --  ┊......┊└┈┈┘└─────┘└┈┈┈┈┘
      --  ┊......┊┌┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┐
      --  ┊......┊┊...............┊
      --  ┊......┊┊...............┊
      --  └┈┈┈┈┈┈┘└┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┘
      it "creates the correct number of tiles" $ do
        length (compactify $ tiles outer is) `shouldBe` 9

    specify "all tiles are in the outer"
      $ property $ \(TileInput outer is) ->
        let ts = tiles outer is
         in all (\r -> inRange outer (lb r) && inRange outer (lb r)) ts

    specify "no tiles are in the intersections"
      $ property $ \(TileInput outer is) ->
        let ts = tiles outer is
         in none (\tile -> any (overlaps tile) is) ts

    specify "no tiles overlap any other"
      $ property $ \(TileInput outer is) ->
        let ts = tiles outer is
         in none (\tile -> any (overlaps tile) (filter (/= tile) ts)) ts

    specify "tiles + intersections cover the full region"
      $ property $ \(TileInput outer is) ->
        let ts = tiles outer is
         in rangeSize outer === sum (fmap rangeSize (ts <> is))

neighboursSpec = describe "neighbours" $ do

  describe "the eight 3x3 boxes around a 3x3 box" $ do
    let ns = neighbours ((0,0), (8,8)) ((3,3), (5,5))
    it "all have 9 locations" $ do
      fmap rangeSize ns `shouldSatisfy` all (== 9)

  specify "all neighbours are well-formed" 
    $ property $ \(NeighbourInput outer inner) ->
      let ns = namedNeighbours outer inner
          test p n = p .&&. counterexample (unwords [show n, "is ill-formed"])
                                           (let r = snd n
                                            in (leftEdge r <= rightEdge r)
                                               && (topEdge r <= btmEdge r))
       in L.foldl' test (property True) ns

  specify "there are at most 8 neighbours"
    $ property $ \(NeighbourInput outer inner) ->
      let ns = neighbours outer inner
       in length ns `elem` [0 .. 8]

  specify "no neighbour overlaps the inner section"
    $ property $ \(NeighbourInput outer inner) ->
      let ns = neighbours outer inner
       in not $ any (overlaps inner) ns

  specify "all neighbours are entirely within outer"
    $ property $ \(NeighbourInput outer inner) ->
      let ns = namedNeighbours outer inner
          test p n = p .&&. counterexample (unwords [show n, "not within outer"])
                                           (contains (snd n) outer)
       in L.foldl' test (property True) ns

  specify "no neighbour is bigger than the outer region"
    $ property $ \(NeighbourInput outer inner) ->
      let ns = namedNeighbours outer inner
          os = rangeSize outer
          max_n_size = L.maximumBy (comparing (rangeSize . snd)) ns
       in not (null ns) ==> counterexample (unwords [show os, "<=", show max_n_size])
                                           (os > rangeSize (snd max_n_size))

  it "accounts for all the locations"
    $ property $ \(NeighbourInput outer inner) ->
      let ns = neighbours outer inner
       in rangeSize outer === sum (rangeSize <$> inner : ns)


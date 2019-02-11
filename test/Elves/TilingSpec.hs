module Elves.TilingSpec where

import Data.Ix
import Data.Ord
import qualified Data.List as L
import Control.Lens.Combinators (none)
import qualified Data.Set as S

import Test.Hspec
import Test.QuickCheck hiding (scale, NonZero, within)

import Elves.Coord hiding (corners)
import Elves.Cartesian
import Elves.Tiling

data NeighbourInput = NeighbourInput (Bounds Location) (Bounds Location)
  deriving (Show, Eq)

data TileInput = TileInput (Bounds Location) [Bounds Location]
  deriving (Show, Eq)

arbBox minx maxx miny maxy = do
  tly <- choose (miny, maxy)
  tlx <- choose (minx, maxx)
  bly <- choose (tly, maxy)
  blx <- choose (tlx, maxx)
  pure ((tly,tlx),(bly,blx))

instance Arbitrary NeighbourInput where
  arbitrary = do
    outer <- arbBox 0 999 0 999
    inner <- arbBox (x $ lb outer) (x $ ub outer) (y $ lb outer) (y $ ub outer)

    return (NeighbourInput outer inner)

instance Arbitrary TileInput where
  arbitrary = do
    outer <- arbBox 0 1000 0 1000
    let inner = arbBox (x $ lb outer) (x $ ub outer) (y $ lb outer) (y $ ub outer)
    is <- listOf1 inner
    pure (TileInput outer (dedup is))
    where
      dedup = L.nubBy overlaps

spec :: Spec
spec = describe "Elves.Tiling" $ do
  tilesSpec
  neighboursSpec

tilesSpec = describe "tiles" $ do

    -- verify the input itself
    -- these are the preconditions to tiles
    describe "TileInput" $ do

      specify "all intersections are entirely within outer" 
        $ property $ \(TileInput outer is) ->
           all (\x -> x `within` outer) is

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
      --- 0000000000111111111122222
      --- 0123456789012345678901234
      --  //////|-----------------+ 00
      --  //////| .   .     .     | 01
      --  //////| .   .     .     | 02
      --  //////|.+...+.....+------ 03
      --  ------+.+...+.....|////// 04
      --  |     . .   .     |////// 05
      --  |     . .   .     |////// 06
      --  |     . .   .     |////// 07
      --  |     . .   .     |////// 08
      --  |     . .   .     |////// 09
      --  |.....+.+...+.... +------ 10
      --  |     . .   .     .     | 11
      --  --------+...+.....+.....| 12
      --  ////////|   .     .     | 13
      --  ////////|   .     .     | 14
      --  ////////|   .     .     | 15
      --  ////////|...+-----+.....| 16
      --  ////////|   |/////|     | 17
      --  ////////|   |/////|     | 18
      --  ////////|   |/////|     | 19
      --  --------+...|/////|.....| 20
      --  |     . .   |/////|     | 21
      --  |.....+.+...+-----+.....| 22
      --  |     . .   .     .     | 23
      --  |     . .   .     .     | 24
      --  |     . .   .     .     | 25
      --  +-----------------------+ 26
      it "creates the correct number of tiles" $ do
        length (tiles outer is) `shouldBe` 12

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
                                           (snd n `within` outer)
       in L.foldl' test (property True) ns

  specify "no neighbour is bigger than the outer region"
    $ property $ \(NeighbourInput outer inner) ->
      let ns = namedNeighbours outer inner
          os = rangeSize outer
          max_n_size = L.maximumBy (comparing (rangeSize . snd)) ns
       in counterexample (unwords [show os, "<=", show max_n_size])
                         (os > rangeSize (snd max_n_size))

  it "accounts for all the locations"
    $ property $ \(NeighbourInput outer inner) ->
      let ns = neighbours outer inner
       in rangeSize outer === sum (rangeSize <$> inner : ns)

showTiles :: Bounds Location -> [Bounds Location] -> [Bounds Location] -> String
showTiles outer intersections tiles
  = unlines $ fmap row [topEdge outer .. btmEdge outer]
  where
    accounted_for = intersections ++ tiles
    all_corners = S.fromList ((outer : intersections ++ tiles) >>= corners)
    solid_edges_hr = (outer : intersections) >>= \r -> [(topLeft r, topRight r)
                                                       ,(btmLeft r, btmRight r)
                                                       ]
    solid_edges_vt = (outer : intersections) >>= \r -> [(topLeft r, btmLeft r)
                                                       ,(topRight r, btmRight r)
                                                       ]
    tile_edges = tiles >>= \r -> [(topLeft r, btmLeft r)
                                 ,(topRight r, btmRight r)
                                 ,(topLeft r, topRight r)
                                 ,(btmLeft r, btmRight r)
                                 ]
    row y = fmap (cell y) [leftEdge outer .. rightEdge outer]
    cell y x
       | none (\r -> inRange r (y,x)) accounted_for = '?'
       | S.member (y,x) all_corners                 = '+'
       | any (\e -> inRange e (y,x)) solid_edges_hr = '-'
       | any (\e -> inRange e (y,x)) solid_edges_vt = '|'
       | any (\i -> inRange i (y,x)) intersections  = '/'
       | any (\e -> inRange e (y,x)) tile_edges     = '.'
       | any (\t -> inRange t (y,x)) tiles          = ' '
       | otherwise                                  = '!'


runExample = do
      let outer = ((0,0),(26,24))
          is = [((0,16),(3,24))
               ,((0,0),(4,6))
               ,((3,18),(10,24))
               ,((12,0),(20,7))
               ,((16,12),(22,18))
               ]
          ts = tiles outer is
      putStrLn (showTiles outer is (compactify ts))

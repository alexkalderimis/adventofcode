{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}

module Elves.CoordSpec (spec) where

import           Control.Monad (forM_)
import           Data.Maybe
import           System.Random
import           Control.Lens    hiding (contains, elements, index)
import qualified Data.Ix         as Ix
import qualified Data.List       as L

import           Test.Hspec
import           Test.QuickCheck hiding (NonZero, scale, within)

import           Elves.Coord

import Support.BoundingBoxes

type Point = Dim3
type Point2 = Dim2
type Point4 = (Int,Int,Int,Int)
type B a = (a,a)

newtype NonZero a = NonZero a deriving Show

instance (Arbitrary a, Coord a) => Arbitrary (NonZero a) where
  arbitrary = NonZero <$> (arbitrary `suchThat` (not . isZero))

isZero :: (Eq a, Coord a) => a -> Bool
isZero = (== origin)

spec :: Spec
spec = describe "Elves.Coord" $ do
  expandSpec
  mindistSpec
  translateSpec
  containsSpec
  overlapsSpec
  straightLineSpec
  manhattanSpec
  closestPointSpec

translateSpec = describe "translate" $ do
  specify "translate dx . translate (invert dx) === id, for 2-tuples" $ property $ \dx px ->
    translate dx (translate (invert dx) px) == (px :: Point2)
  specify "translate dx . translate (invert dx) === id, for 3-tuples" $ property $ \dx px ->
    translate dx (translate (invert dx) px) == (px :: Point)
  specify "translate dx . translate (invert dx) === id, for 4-tuples" $ property $ \dx px ->
    translate dx (translate (invert dx) px) == (px :: Point4)
  specify "translation by non-zero always causes changes" $ property $ \(NonZero dx) px ->
    translate dx px /= (px :: Point)
  specify "translation by non-zero always causes changes, for 4-tuples" $ property $ \(NonZero dx) px ->
    translate dx px /= (px :: Point4)
  specify "translation by non-zero always causes changes, for 2-tuples" $ property $ \(NonZero dx) px ->
    translate dx px /= (px :: Point2)
  specify "translation by zero never causes changes" $ property $ \px ->
    translate (0,0,0) px == (px :: Dim3)
  specify "translate dx . translate dx == translate (dbl dx)" $ property $ \dx px ->
    translate dx (translate dx px) == translate (scale 2 dx) (px :: Point)

containsSpec = describe "contains" $ do
  describe "1D" $ do
    -- A [-------------]
    -- B       [----------]
    -- C   [------]
    -- D                [----------]
    let a = (0, 10) :: Dim2
        b = (5, 12)
        c = (3, 7)
        d = (11, 17)
    forM_ [a, b, c, d] $ \x -> specify (show x <> " is within itself") $ do
      x `shouldSatisfy` contains x
    forM_ [b, d] $ \x -> specify (show x <> " is not within A") $ do
      a `shouldNotSatisfy` contains x
    specify "C is within A" $
      a `shouldSatisfy` contains c
    specify "A is not within C" $
      c `shouldNotSatisfy` contains a

  describe "2D" $ do
    -- 00    +------+ ----- A
    -- 01    |      |
    -- 02 +----------+ ---- B
    -- 03 |          |
    -- 04 |    +---+ | ---- C
    -- 05 |    | +-|-| ---- Q
    -- 06 |    +---+ |   
    -- 07 |      |   |   
    -- 08 +----------+
    -- 09    |      |
    -- 10    +------+
    -- 11
    -- 12 +----------+ ---- D
    -- 13 |          |
    -- 14 +----------+
    --   
    --    0----5----1---------2
    --
    let a = ((3, 0), (10, 10))
        b = ((0, 2), (11,  8))
        c = ((5, 4), (9, 6))
        d = ((0, 12), (11, 14))
        q = ((7, 5), (11, 8))

    specify "A contains C" $ do
      a `shouldSatisfy` contains c
    specify "C does not contain A" $ do
      c `shouldNotSatisfy` contains a

    specify "all points in C are in A" $
      Ix.range c `shouldSatisfy` all (Ix.inRange a)

    specify "not all points in A are in C" $ do
      Ix.range a `shouldNotSatisfy` all (Ix.inRange c)

    specify "if region does not contain another, then we can find an element not within it" $
      property $ \(Region x) (Region y) ->
         not (contains x y) ==> not (all (Ix.inRange y) (Ix.range x))

    specify "A does not contain Q" $ do
      a `shouldNotSatisfy` contains q
    specify "B contains C and Q" $ do
      b `shouldSatisfy` contains c
      b `shouldSatisfy` contains q
    specify "C is within itself" $ do
      c `shouldSatisfy` contains c
    specify "D does not contain any region but itself" $ do
      d `shouldSatisfy` contains d
      forM_ [a, b, c, q] $ \x -> d `shouldNotSatisfy` contains x
    specify "A does not contain B or vice versa" $ do
      a `shouldNotSatisfy` contains b
      b `shouldNotSatisfy` contains a
    specify "C and Q do not contain each other" $ do
      c `shouldNotSatisfy` contains q
      q `shouldNotSatisfy` contains c

  describe "3D" $ do
    specify "all points in cube are entirely within it" $
      property $ \(CubeWithPoint (Cube c) p) -> contains (p,p) c

    specify "cube in cube means it is contained" $
      property $ \(CubeWithCube (Cube container) (Cube c)) ->
        container `shouldSatisfy` contains c

    specify "if region does not contain another, then we can find an element not within it" $
      property $ \(Cube x) (Cube y) ->
        not (contains x y) ==> not (all (Ix.inRange y) (Ix.range x))

    it "knows that (-3,-3,-3),(3,3,3) is not within (0,-4,0),(0,4,0)" $ do
      ((0,-4,0),(0,4,0)) `shouldNotSatisfy` contains ((-3,-3,-3),(3,3,3))
    it "knows that (0,-4,0),(0,4,0) is not within (-3,-3,-3),(3,3,3)" $ do
      ((-3,-3,-3),(3,3,3)) `shouldNotSatisfy` contains ((0,-4,0),(0,4,0))


overlapsSpec = describe "overlaps" $ do
    --     +----+
    --     |    |
    --  +----------+
    --  |          |
    --  +----------+
    --     |    |
    --     +----+
    specify "((4,1),(8,10) overlaps ((0,5),(12,8))" $ do
      ((4,1),(8,10)) `shouldSatisfy` overlaps ((0,5),(12,8))
    --     +----+
    --     |    |
    --     |    | +----------+
    --     |    | |          |
    --     |    | +----------+
    --     |    |
    --     +----+
    specify "((4,1),(8,10) does not overlap ((9,5),(12,8))" $ do
      ((4,1),(8,10)) `shouldNotSatisfy` overlaps ((9,5),(12,8))
    --     +--------+
    --     |        |
    --     | +--+   |
    --     | |  |   |
    --     | +--+   |
    --     +--------+
    specify "((4,1),(8,10) does overlap ((5,3),(7,6))" $ do
      ((4,1),(8,10)) `shouldSatisfy` overlaps ((5,3),(7,6))
    it "is commutative" $ property $ \(Cube a) (Cube b) ->
      overlaps a b == overlaps b (a :: B Point)
    it "means that at least one point in a is in b" $ property $ \(Cube a) (Cube b) ->
      overlaps a b == any (Ix.inRange a) (Ix.range (b :: B Point))

    let lhs = ((0,-4,0),(0,4,0))
        rhs = ((-3,-3,-3),(3,3,3))
     in specify (show lhs ++ " overlaps " ++ show rhs) $ do
         lhs `shouldSatisfy` overlaps rhs

    let lhs = ((0,-4,0,0),(0,4,0,0))
        rhs = ((-3,-3,-3,-3),(3,3,3,3))
     in specify (show lhs ++ " overlaps " ++ show rhs) $ do
         lhs `shouldSatisfy` overlaps rhs

    specify "all cubes that are within other cubes also overlap" $
      property $ \(CubeWithCube (Cube a) (Cube b)) -> overlaps a b && overlaps b a

straightLineSpec = describe "straightLine" $ do
    let zero = 0 :: Double
    specify "distance is always >= 0, for 3-tuples" $ property $ \a b ->
      straightLine a (b :: Point) >= zero
    specify "distance is always > 0, for distinct 3-tuples" $ property $ \a b ->
      a == b || straightLine a (b :: Point) > zero
    specify "distance is always == 0, for identical 3-tuples" $ property $ \a b ->
      a /= b || straightLine a (b :: Point) == zero
    specify "distance is always >= 0, for 4-tuples" $ property $ \a b ->
      straightLine a (b :: Point4) >= zero
    it "knows the distance from (5,5) to (8,9)" $ do
      straightLine (5,5) ((8,9) :: Point2) `shouldBe` (5 :: Double)
    it "knows the distance from (-2,3) to (7,10)" $ do
      straightLine (-2,3) ((7,10) :: Point2) `shouldBe` (sqrt ((9 ^ 2) + (7 ^ 2)) :: Double)

manhattanSpec = describe "manhattan" $ do
    specify "distance is always >= 0, for 3-tuples" $ property $ \a b ->
      manhattan a (b :: Point) >= 0
    specify "distance is always > 0, for distinct 3-tuples" $ property $ \a b ->
      a == b || manhattan a (b :: Point) > 0
    specify "distance is always == 0, for identical 3-tuples" $ property $ \a b ->
      a /= b || manhattan a (b :: Point) == 0
    specify "distance is always >= 0, for 4-tuples" $ property $ \a b ->
      manhattan a (b :: Point4) >= 0
    it "knows the distance from (5,5) to (10,10)" $ do
      manhattan (5,5) ((10,10) :: Point2) `shouldBe` 10
    it "knows the distance from (5,5) to (8,9)" $ do
      manhattan (5,5) ((8,9) :: Point2) `shouldBe` 7
    it "knows the distance from (-2,3) to (7,10)" $ do
      manhattan (-2,3) ((7,10) :: Point2) `shouldBe` 16
    it "is the sum of the straightline distances" $ property $ \a b ->
      manhattan a (b :: Point) == sum (mindists (a,a) (b,b))

closestPointSpec = describe "closestPoint" $ do
    specify "The closest point is closer by any measure, in 1D" $
      withMaxSuccess 10000 $
      forAll arbitrary                  $ \(Range r) ->
      forAll (chooseInt (fst r, snd r)) $ \x -> \h p ->
        let cp = closestPoint p r
         in measure h p cp <= measure h p x

    specify "The closest point is closer by any measure, in 2D" $
      withMaxSuccess 10000 $
      forAll arbitrary                              $ \(Region r) ->
      forAll (chooseInt (fst (fst r), fst (snd r))) $ \x ->
      forAll (chooseInt (snd (fst r), snd (snd r))) $ \y -> \h p ->
        let cp = closestPoint p r
         in measure h p cp <= measure h p (x,y)

    specify "The closest point is closer by any measure, in 3D" $
      withMaxSuccess 1000 $ \h p (CubeWithPoint (Cube c) point)->
        let cp = closestPoint p c
         in measure h p cp <= measure h p point

mindistSpec = describe "mindist" $ do
  let shouldBeDbl a b = abs (a - b) `shouldSatisfy` (< (0.000001 :: Double))
  specify "mindist (0,0,7) ((-1,-4,-12),(-1,4,-12)) == 19.02.." $ do
    let p = (0,0,7) :: (Int, Int, Int)
        b = ((-1,-4,-12),(-1,4,-12))
        x = (-1,0,-12)
     in mindist p b `shouldBeDbl` straightLine p x

  let box = ((3,4), (6,10)) :: ((Int, Int), (Int, Int))
      zero = 0 :: Double
  specify "the mindist is always >= 0" $ property $ \p (Cube b) ->
      mindist (p :: Point) b >= zero
  describe "for points contained in the search box" $ do
    describe "the mindist to (5,5)" $ do
      let this = mindist (5,5) box
      it "is zero" $ this `shouldBeDbl` zero
    describe "the mindist to (6,9)" $ do
      let this = mindist (6,9) box
      it "is zero" $ this `shouldBeDbl` zero
    specify "the mindist == 0 if within" $
      property $ \(CubeWithPoint (Cube c) p) -> mindist p c `shouldBeDbl` 0.0
    specify "mindist > 0 if not within" $
      property $ \(Cube c) p ->
        not (contains (p,p) c) ==> mindist p c > zero
  describe "points that differ only on one dimension" $ do
    describe "the mindist to (4,0)" $ do
      let this = mindist (4,20) box
      it "is ten" $ this `shouldBeDbl` 10
    describe "the mindist to (4,13)" $ do
      let this = mindist (4,13) box
      it "is three" $ this `shouldBeDbl` 3
    specify "the mindist to (-1,5) is four" $ do
      mindist (-1,5) box `shouldBeDbl` 4
    specify "the mindist to (12, 7) is six" $ do
      mindist (12,7) box `shouldBeDbl` 6
  describe "points that differ in two or more dimensions" $ do
    specify "the mindist to (0,2) is five" $ do
      mindist (0,0) box `shouldBeDbl` 5 -- using 3,4,5
    specify "the mindist to (11,-8) is thirteen" $ do
      mindist (11,-8) box `shouldBeDbl` 13 -- using 5,12,13
    specify "the mindist to (25,20) is 29" $ do
      mindist (-18,30) box `shouldBeDbl` 29 -- using 20,21,29

expandSpec = describe "expandB" $ do
    it "is commutative" $ property $ \(Cube a) (Cube b) -> expandB a b === expandB b a

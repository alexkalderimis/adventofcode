module Elves.CoordSpec (spec) where

import qualified Data.Ix as Ix
import qualified Data.List as L
import           Control.Lens              hiding (contains, index)

import Test.Hspec
import Test.QuickCheck hiding (scale, NonZero, within)

import Elves.Coord

type Point2 = (Int,Int)
type Point  = (Int,Int,Int)
type Point4 = (Int,Int,Int,Int)

newtype NonZero a = NonZero a deriving Show

instance (Arbitrary a, Coord a) => Arbitrary (NonZero a) where
  arbitrary = NonZero <$> (arbitrary `suchThat` (not . isZero))

newtype Cube a = Cube (a,a) deriving Show

instance (Coord a, Arbitrary a) => Arbitrary (Cube a) where
  arbitrary = do
    lb  <- arbitrary
    ubs <- sequence [arbitrary `suchThat` (>= x) | Lens d <- dimensions, let x = lb ^. d]
    return (Cube (lb, L.foldl' (\c (Lens d, v) -> set d v c) origin (zip dimensions ubs)))

isZero :: Coord i => i -> Bool
isZero c = all ((0 ==) . (c ^.) . runLens) dimensions

spec :: Spec
spec = describe "Elves.Coord" $ do
  describe "translate" $ do
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
      translate (0,0,0) px == (px :: Point)
    specify "translate dx . translate dx == translate (dbl dx)" $ property $ \dx px ->
      translate dx (translate dx px) == translate (scale 2 dx) (px :: Point)

  describe "overlaps" $ do
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
      overlaps a b == overlaps b (a :: (Point,Point))
    it "means that at least one point in a is in b" $ property $ \(Cube a) (Cube b) ->
      let implies = if overlaps a b then id else not
       in implies $ any (Ix.inRange a) (Ix.range (b :: (Point,Point)))

  describe "straightLine" $ do
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

  describe "manhattan" $ do
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

  describe "mindist" $ do
    let box = ((3,4), (6,10))
    specify "the mindist is always >= 0" $ property $ \p (Cube b) ->
        mindist (p :: Point) b >= 0
    describe "for points contained in the search box" $ do
      describe "the mindist to (5,5)" $ do
        let this = mindist (5,5) box
        it "is zero" $ this `shouldBe` 0
      describe "the mindist to (6,9)" $ do
        let this = mindist (6,9) box
        it "is zero" $ this `shouldBe` 0
      specify "the mindist == 0 iff within" $ property $ \p (Cube b) ->
        let implies = if mindist (p :: Point) b == 0 then id else not
         in implies $ within (p,p) b
    describe "points that differ only on one dimension" $ do
      describe "the mindist to (4,0)" $ do
        let this = mindist (4,20) box
        it "is ten" $ this `shouldBe` 10
      describe "the mindist to (4,13)" $ do
        let this = mindist (4,13) box
        it "is three" $ this `shouldBe` 3
      specify "the mindist to (-1,5) is four" $ do
        mindist (-1,5) box `shouldBe` 4
      specify "the mindist to (12, 7) is six" $ do
        mindist (12,7) box `shouldBe` 6
    describe "points that differ in two or more dimensions" $ do
      specify "the mindist to (0,2) is five" $ do
        mindist (0,0) box `shouldBe` 5 -- using 3,4,5
      specify "the mindist to (11,-8) is thirteen" $ do
        mindist (11,-8) box `shouldBe` 13 -- using 5,12,13
      specify "the mindist to (25,20) is 29" $ do
        mindist (-18,30) box `shouldBe` 29 -- using 20,21,29

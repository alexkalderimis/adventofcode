module Elves.CoordSpec (spec) where

import           Control.Lens              hiding (contains, index)

import Test.Hspec
import Test.QuickCheck hiding (scale, NonZero)

import Elves.Coord

type Point2 = (Int,Int)
type Point  = (Int,Int,Int)
type Point4 = (Int,Int,Int,Int)

newtype NonZero a = NonZero a deriving Show

instance (Arbitrary a, Coord a) => Arbitrary (NonZero a) where
  arbitrary = NonZero <$> (arbitrary `suchThat` (not . isZero))

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
  describe "manhattan" $ do
    specify "distance is always >= 0, for 3-tuples" $ property $ \a b ->
      manhattan a (b :: Point) >= 0
    specify "distance is always > 0, for distinct 3-tuples" $ property $ \a b ->
      a == b || manhattan a (b :: Point) > 0
    specify "distance is always == 0, for identical 3-tuples" $ property $ \a b ->
      a /= b || manhattan a (b :: Point) == 0
    specify "distance is always >= 0, for 4-tuples" $ property $ \a b ->
      manhattan a (b :: Point4) >= 0


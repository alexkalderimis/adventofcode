module Elves.IntervalSpec (spec) where

import qualified Data.Ix as Ix
import qualified Data.List as L
import           Control.Lens              hiding (contains, index)

import Test.Hspec
import Test.QuickCheck hiding (scale, NonZero, within)

import Elves.Interval

type Point2 = (Int,Int)
type Point  = (Int,Int,Int)
type Point4 = (Int,Int,Int,Int)

spec :: Spec
spec = describe "Elves.Interval" $ do
  describe "overlaps" $ do
    --     +----+
    --     |    |
    --  +----------+
    --  |          |
    --  +----------+
    --     |    |
    --     +----+
    specify "((4,1),(8,10) overlaps ((0,5),(12,8))" $ do
      Interval (4,1) (8,10) `shouldSatisfy` overlaps (Interval (0,5) (12,8))
    --     +----+
    --     |    |
    --     |    | +----------+
    --     |    | |          |
    --     |    | +----------+
    --     |    |
    --     +----+
    specify "((4,1),(8,10) does not overlap ((9,5),(12,8))" $ do
      Interval (4,1) (8,10) `shouldNotSatisfy` overlaps (Interval (9,5) (12,8))
    --     +--------+
    --     |        |
    --     | +--+   |
    --     | |  |   |
    --     | +--+   |
    --     +--------+
    specify "((4,1),(8,10) does overlap ((5,3),(7,6))" $ do
      Interval (4,1) (8,10) `shouldSatisfy` overlaps (Interval (5,3) (7,6))
    it "is commutative" . property $ \a b ->
      overlaps a b === overlaps b (a :: Interval Point)
    it "means that at least one point in a is in b - 2D" . property $ \a b ->
      overlaps a b === any (inRange a) (range (b :: Interval Point2))
    it "means that at least one point in a is in b - 3D" . property $ \a b ->
      overlaps a b === any (inRange a) (range (b :: Interval Point))

    let lhs = Interval (0,-4,0) (0,4,0)
        rhs = Interval (-3,-3,-3) (3,3,3)
     in specify (show lhs <> " overlaps " <> show rhs) $ do
         lhs `shouldSatisfy` overlaps rhs

    let lhs = Interval (0,-4,0,0) (0,4,0,0)
        rhs = Interval (-3,-3,-3,-3) (3,3,3,3)
     in specify (show lhs <> " overlaps " <> show rhs) $ do
         lhs `shouldSatisfy` overlaps rhs

  describe "contains" $ do

    specify "contains implies overlaps" . property $ \a b ->
      contains a b ==> overlaps a (b :: Interval Int)

    it "means that all points of a are in b" . property $ \a b ->
      contains a b === all (inRange b) (range (a :: Interval Point))
    it "means that all points of a are in b-1D" . property $ \a b ->
      contains a b === all (inRange b) (range (a :: Interval Int))


module ElvesSpec (spec) where

import Test.Hspec
import Test.QuickCheck

import Elves

newtype IncreasingRange = IncreasingRange (Int, Int) deriving Show

instance Arbitrary IncreasingRange where
  arbitrary = do
    lb <- arbitrary
    ub <- arbitrary `suchThat` (>= lb)
    return (IncreasingRange (lb,ub))

data RangeWithInflection = RangeWithInflection IncreasingRange Int deriving Show

instance Arbitrary RangeWithInflection where
  arbitrary = do
    (IncreasingRange bs) <- arbitrary
    point <- choose bs
    return (RangeWithInflection (IncreasingRange bs) point)

spec :: Spec
spec = do
  describe "minimalBinarySearch" $ do
    it "finds the point in a monotonic range" $ do
      property $ \(RangeWithInflection (IncreasingRange bs) pnt) ->
        pnt == minimalBinarySearch (>= pnt) bs
  describe "maximalBinarySearch" $ do
    it "finds the point in a monotonic range" $ do
      property $ \(RangeWithInflection (IncreasingRange bs) pnt) ->
        pnt == maximalBinarySearch (<= pnt) bs
  describe "boundedSearch" $ do
    it "finds a suitable value even when binary helpers are not great" $ do
      property $ \(RangeWithInflection (IncreasingRange bs) pnt) ->
        Just pnt == boundedSearch (< pnt) (> pnt) (== pnt) bs
    it "cannot find items excluded by the predicates" $ do
      property $ \(RangeWithInflection (IncreasingRange bs) pnt) ->
        Nothing == boundedSearch (< pnt) (> pnt) (== (pnt - 2)) bs


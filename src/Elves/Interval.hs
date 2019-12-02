{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE UndecidableInstances #-}

module Elves.Interval where

import           Control.Lens              hiding (contains)
import           Data.Ix                   (Ix)
import qualified Data.Ix                   as Ix
import           Test.QuickCheck.Arbitrary (Arbitrary (arbitrary, shrink),
                                            arbitraryBoundedEnum)
import           Test.QuickCheck.Gen       (suchThat)

import           Elves.Coord               (Coord, Overlapping, Dimension, dimensions, mindists)
import qualified Elves.Coord               as Coord

data Interval a = Interval { low :: a, hi :: a } deriving Show

inRange :: Ix a => Interval a -> a -> Bool
inRange (Interval lo hi) = Ix.inRange (lo,hi)

range :: Ix a => Interval a -> [a]
range (Interval lo hi) = Ix.range (lo,hi)

size :: Ix a => Interval a -> Int
size (Interval lo hi) = Ix.rangeSize (lo,hi)

instance (Arbitrary i, Ord (Dimension i), Coord i) => Arbitrary (Interval i) where
  arbitrary = do
    lb <- arbitrary
    ub <- arbitrary `suchThat` allDimensionsAbove lb
    return (Interval lb ub)
   where
     allDimensionsAbove lb ub = all (\(Lens d) -> lb^.d <= ub^.d) dimensions

point :: Ord a => a -> Interval a
point x = Interval x x

contains :: Ix a => Interval a -> Interval a -> Bool
contains (Interval a b) bigger = inRange bigger a && inRange bigger b

overlaps :: (Overlapping a) => Interval a -> Interval a -> Bool
overlaps (Interval a b) (Interval c d) = Coord.overlaps (a,b) (c,d)

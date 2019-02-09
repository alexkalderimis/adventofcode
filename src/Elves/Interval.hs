module Elves.Interval where

import qualified Data.Ix as Ix
import           Data.Ix (Ix)
import           Control.Lens              hiding (contains)
import           Test.QuickCheck.Arbitrary (Arbitrary (arbitrary, shrink),
                                            arbitraryBoundedEnum)
import           Test.QuickCheck.Gen       (suchThat)

import Elves.Coord (Coord, dimensions, mindists)

-- ideally it would be nice to avoid the Ix type so as to handle
-- non integral co-ordinates. That means replacing the functionality
-- of Ix in Coord
data Interval a = Interval { low :: a, hi :: a } deriving Show

inRange :: Ix a => Interval a -> a -> Bool
inRange (Interval lo hi) = Ix.inRange (lo,hi)

range :: Ix a => Interval a -> [a]
range (Interval lo hi) = Ix.range (lo,hi)

size :: Ix a => Interval a -> Int
size (Interval lo hi) = Ix.rangeSize (lo,hi)

instance (Arbitrary i, Coord i) => Arbitrary (Interval i) where
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

-- do a and b share any points?
overlaps :: Coord a => Interval a -> Interval a -> Bool
overlaps a b = all (== 0) (mindists (low a,hi a) (low b, hi b))

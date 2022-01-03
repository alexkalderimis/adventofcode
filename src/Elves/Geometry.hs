{-# LANGUAGE TypeFamilies      #-}

module Elves.Geometry where

import Prelude hiding (zipWith)

import qualified Data.List                       as L
import qualified Data.Vector as V
import           Data.Vector (Vector)
import           Data.Tree                       (Tree (..))
import           Control.Lens    (at, _1, _2, _3, iso, Lens', ReifiedLens(..), lens)
import Data.Coerce (coerce)
import Control.Lens.Iso (coerced)

import Elves.Collections
import Elves.Coord (Coord(..))
import           Test.QuickCheck.Arbitrary

-- nicer names for these things
type Point a = Vector a
type Region a = [Point a]

data Plane a = Plane a (Point a)

point :: [a] -> Point a
point = V.fromList

-- unsafe lenses
pointX, pointY, pointZ :: Lens' (Point a) a
pointX = lens (\p -> p V.! 0) (\p x -> p V.// [(0, x)])
pointY = lens (\p -> p V.! 1) (\p y -> p V.// [(1, y)])
pointZ = lens (\p -> p V.! 2) (\p z -> p V.// [(2, z)])

newtype Point3 a = P3 { unpoint3 :: Point a } deriving (Show, Eq, Ord)

instance (Num a, Ord a) => Coord (Point3 a) where
  type Dimension (Point3 a) = a
  origin = P3 (point [0, 0, 0])
  dimensions = [ Lens (coerced . pointX)
               , Lens (coerced . pointY)
               , Lens (coerced . pointZ)
               ]

instance Arbitrary a => Arbitrary (Point3 a) where
  arbitrary = do (x, y, z) <- arbitrary
                 pure . P3 $ point [x, y, z]

-- Bunch of functions for dealing with N-dimensional geometry:

midpoint :: (Fractional a, Zips f) => f a -> f a -> f a
midpoint = zipWith mid where mid a b = (a + b) / 2

regionCentre :: Fractional a => Region a -> Point a
regionCentre es = V.fromList . fmap avg . L.transpose $ fmap V.toList es
  where avg p = sum p / n
        n = fromIntegral $ length es

-- expand the region by a new dimension
extrema :: Num a => a -> Region a -> Region a
extrema n [] = [pure n]
extrema n es = V.cons n zeros : fmap (V.cons 0) es
  where zeros = V.fromList $ pure 0 <$> es

-- Split a region (defined by extrema) into a set of sub-regions
split :: Eq a => Region a -> Point a -> [Region a]
split reg p = fmap (\e -> p : filter (/= e) reg) reg

-- generate just the points in the plane, without even
-- considering any other irrelevant points.
--
-- Returns the points that sum to a total a, ignoring negative quadrants.
planePoints :: (Num a, Enum a, Cons f, Monoid (f a), Applicative f)
            => a -> Word -> [f a]
planePoints total dimensionality = untree $ summingTree total dimensionality
  where
    summingTree t 0 = []
    summingTree t 1 = [Node t []]
    summingTree t n = fmap (\x -> Node x $ summingTree (t - x) (n - 1)) [0 .. t]

    untree [] = [mempty]
    untree ts = ts >>= \(Node x rst) -> fmap (cons x) (untree rst)


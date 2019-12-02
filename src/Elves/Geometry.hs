module Elves.Geometry where

import Prelude hiding (zipWith)

import qualified Data.List                       as L
import qualified Data.Vector as V
import           Data.Vector (Vector)
import           Data.Tree                       (Tree (..))

import Elves.Collections

-- nicer names for these things
type Point a = Vector a
type Region a = [Point a]

data Plane a = Plane a (Point a)

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
planePoints :: (Num a, Enum a, Cons f, Monoid (f a), Applicative f)
            => a -> Word -> [f a]
planePoints total dimensionality = untree $ summingTree total dimensionality
  where
    summingTree t 0 = []
    summingTree t 1 = [Node t []]
    summingTree t n = fmap (\x -> Node x $ summingTree (t - x) (n - 1)) [0 .. t]

    untree [] = [mempty]
    untree ts = ts >>= \(Node x rst) -> fmap (cons x) (untree rst)


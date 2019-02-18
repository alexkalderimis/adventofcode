module Elves.Geometry where

import qualified Data.List                       as L

-- nicer names for these things
type Point a = [a]
type Region a = [Point a]

-- Bunch of functions for dealing with N-dimensional geometry:

midpoint :: [Double] -> [Double] -> [Double]
midpoint = zipWith (\a b -> (a + b) / 2)

regionCentre :: Fractional a => Region a -> Point a
regionCentre es = let n = fromIntegral $ length es
                   in [sum p / n | p <- L.transpose es]

-- expand the region by a new dimension
extrema :: Num a => a -> Region a -> Region a
extrema n [] = [[n]]
extrema n es = (n : fmap (pure 0) es) : fmap (0 :) es

-- Split a region (defined by extrema) into a set of sub-regions
split :: Eq a => Region a -> Point a -> [Region a]
split reg p = fmap (\e -> p : filter (/= e) reg) reg



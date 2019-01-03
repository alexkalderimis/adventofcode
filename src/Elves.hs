module Elves (
  pairs,
  getMatrix,
  minimalBinarySearch,
  module Hspec,
  module Attoparsec
  ) where

import Data.List
import Test.Hspec as Hspec
import Data.Attoparsec.Text as Attoparsec (Parser, parseOnly)

pairs :: [a] -> [(a, a)]
pairs l = [(x,y) | (x:ys) <- tails l, y <- ys]

getMatrix :: (Read a) => IO [[a]]
getMatrix = fmap (fmap read . words) . lines <$> getContents

minimalBinarySearch :: (Int -> Bool) -> (Int, Int) -> Int
minimalBinarySearch isOK = go
  where
    go rng | rngSize rng < 2 = fst rng
           | otherwise = let p = mid rng
                             rng' = if isOK p then (fst rng, p) else (p + 1, snd rng)
                          in go rng'
    rngSize (a,b) = (b - a) + 1
    mid rng = let n = rngSize rng in fst rng + (div n 2 - if even n then 1 else 0)

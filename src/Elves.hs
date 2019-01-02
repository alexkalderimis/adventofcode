module Elves (
  pairs,
  getMatrix,
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

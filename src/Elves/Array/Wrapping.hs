{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Elves.Array.Wrapping (Wrapping(..)) where

import Data.Coerce
import Data.Array (Array, Ix)
import qualified Data.Array as Array

class Ix i => Wrapping i where
  wrap :: (i, i) -> i -> i

newtype Numeric a = Numeric a deriving (Num, Ord, Eq, Ix, Integral, Real, Enum)

instance (Num i, Integral i, Ix i) => Wrapping (Numeric i) where
  wrap (a, b) x | Array.inRange (a, b) x = x
  wrap (a, b) x | x < a = let w = Numeric . fromIntegral $ Array.rangeSize (a, b)
                              x' = (a - x) `mod` w
                           in b - x'
  wrap (a, b) x = let w = Array.rangeSize (a, b)
                   in (a + x) `mod` (Numeric . fromIntegral $ w)

instance Wrapping Int where
  wrap = coerce (wrap :: (Numeric Int, Numeric Int) -> Numeric Int -> Numeric Int)

instance Wrapping Word where
  wrap = coerce (wrap :: (Numeric Word, Numeric Word) -> Numeric Word -> Numeric Word)

instance Wrapping Integer where
  wrap = coerce (wrap :: (Numeric Integer, Numeric Integer) -> Numeric Integer -> Numeric Integer)

instance (Wrapping a, Wrapping b) => Wrapping (a, b) where
  wrap ((a, a'), (b, b')) (x, y) = (wrap (a, b) x, wrap (a', b') y)


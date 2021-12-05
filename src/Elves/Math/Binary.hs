{-# LANGUAGE PatternSynonyms, GeneralizedNewtypeDeriving #-}

module Elves.Math.Binary (
  fromBinary, mkBinary, mkBit,
  Binary(bits), Bit(bitIsSet),
  zero, one
  ) where

import qualified Data.List as L
import qualified Data.Bits as B
import Data.Bits ((.|.), (.&.), complement, shiftL, xor)

newtype Bit = Bit { bitIsSet :: Bool } deriving (Eq, Show, B.Bits)

data Binary = Binary { bits :: ![Bit] }

pattern One = Bit True
pattern Zero = Bit False

mkBinary :: [Bit] -> Binary
mkBinary = Binary

mkBit :: Bool -> Bit
mkBit = Bit

instance Show Binary where
  show = map c . bits
    where c One = '1'
          c Zero = '0'

instance Eq Binary where
  a == b = dropWhile (== zero) (bits a) == dropWhile (== zero) (bits b)

instance B.Bits Binary where
  a .&. b = case norm a b of
              (a', b') -> Binary $ zipWith (.&.) (bits a') (bits b')
  a .|. b = case norm a b of
              (a', b') -> Binary $ zipWith (.|.) (bits a') (bits b')
  a `xor` b = case norm a b of
              (a', b') -> Binary $ zipWith xor (bits a') (bits b')
  complement = Binary . dropWhile (== zero) . map complement . bits
  shift b 0 = b
  shift b i = if i > 0
                 then b { bits = bits b <> replicate i Zero }
                 else b { bits = drop (negate i) (bits b) }
  rotate b 0 = b
  rotate b i = let i' = if i > 0 then i else size b - (negate i)
                   pref = take i (bits b)
                   suff = drop i (bits b)
                in Binary (suff <> pref)
  zeroBits = Binary []
  bit i = Binary (one : replicate (i - 1) zero)
  isSigned _ = False
  bitSize = undefined
  bitSizeMaybe _ = Nothing
  popCount = length . filter (== one) . bits
  testBit x i = let index = size x - i
                 in if index < 0 then False
                                 else bitIsSet (bits x !! index)

fromBinary :: Binary -> Integer
fromBinary = L.foldl' f 0 . bits
  where f r Zero = B.shiftL r 1
        f r One = B.shiftL r 1 .|. 1

one :: Bit
one = Bit True

zero :: Bit
zero = Bit False

size :: Binary -> Int
size = length . dropWhile (== Zero) . bits

pad :: Int -> Binary -> Binary
pad n b = let needed = max 0 (n - length (bits b))
          in Binary { bits = take n (replicate needed Zero <> bits b) }

norm :: Binary -> Binary -> (Binary, Binary)
norm a b = case size a `compare` size b of
  LT -> (pad (size b) a, b)
  EQ -> (a, b)
  GT -> (a, pad (size a) b)

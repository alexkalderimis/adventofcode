{-# LANGUAGE BangPatterns #-}

module Elves.CircularBuffer where

import qualified Data.Foldable as Foldable (toList)
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq

data Buffer a = Buffer { bufIdx :: !Int, getBuffer :: !(Seq a) }
  deriving (Show, Eq)

right :: Buffer a -> Buffer a
right = shift 1

left :: Buffer a -> Buffer a
left = shift (-1)

focus :: Buffer a -> a
focus = Seq.index <$> getBuffer <*> bufIdx

singleton :: a -> Buffer a
singleton a = Buffer 0 (pure a)

fromList :: [a] -> Buffer a
fromList xs = Buffer 0 (Seq.fromList xs)

rewind :: Buffer a -> Buffer a
rewind b = b { bufIdx = 0 }

shift :: Int -> Buffer a -> Buffer a
shift n (Buffer i b) = Buffer j b
  where
    l = Seq.length b
    j = case (i + n) `mod` l of
         x | x < 0 -> l + x
         x         -> x

insertR :: a -> Buffer a -> Buffer a
insertR a (Buffer i b) = Buffer i (Seq.insertAt (succ i) a b)

insertL :: a -> Buffer a -> Buffer a
insertL a (Buffer i b) = Buffer (succ i) (Seq.insertAt i a b)

toList :: Buffer a -> [a]
toList = Foldable.toList . getBuffer

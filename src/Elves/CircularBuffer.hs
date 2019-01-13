{-# LANGUAGE BangPatterns #-}

module Elves.CircularBuffer where

import Data.Vector (Vector)
import qualified Data.Vector as V

data Buffer a = Buffer { bufIdx :: !Int, getBuffer :: !(Vector a) }
  deriving (Show, Eq)

right :: Buffer a -> Buffer a
right = shift 1

left :: Buffer a -> Buffer a
left = shift (-1)

focus :: Buffer a -> a
focus = (V.!) <$> getBuffer <*> bufIdx

singleton :: a -> Buffer a
singleton a = Buffer 0 (pure a)

fromList :: [a] -> Buffer a
fromList xs = Buffer 0 (V.fromList xs)

rewind :: Buffer a -> Buffer a
rewind b = b { bufIdx = 0 }

shift :: Int -> Buffer a -> Buffer a
shift n (Buffer i v) = Buffer j v
  where
    j = case (i + n) `mod` V.length v of
         x | x < 0 -> V.length v + x
         x         -> x

insertR :: a -> Buffer a -> Buffer a
insertR a (Buffer i v) = let ls = V.slice 0 (i + 1) v
                             rs = if succ i == V.length v
                                     then V.empty
                                     else V.slice (succ i) (V.length v - V.length ls) v
                          in Buffer i (V.force (ls <> V.singleton a <> rs))

insertL :: a -> Buffer a -> Buffer a
insertL a (Buffer i v) = let ls = V.slice 0 i v
                             rs = V.slice i (V.length v - V.length ls) v
                          in Buffer (succ i) (V.force (ls <> V.singleton a <> rs))

toList :: Buffer a -> [a]
toList = V.toList . getBuffer

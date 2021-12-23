{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE BangPatterns #-}

module Elves.CircularBuffer where

import           Control.Comonad
import           Control.Comonad.Store
import qualified Data.Foldable as Foldable (toList)
import Data.Sequence (Seq(..))
import qualified Data.Sequence as Seq

data Buffer a = Buffer { bufIdx :: !Int, getBuffer :: !(Seq a) }
  deriving (Show, Eq)

instance Functor Buffer where
  fmap f b = b { getBuffer = fmap f (getBuffer b) }

instance Comonad Buffer where
  extract = focus
  duplicate b = b { getBuffer = Seq.fromFunction (size b) (flip seek b) }

instance ComonadStore Int Buffer where
  pos = bufIdx
  peek i = extract . seek i
  seeks f b = seek (f $ bufIdx b) b

  {-# INLINABLE seek #-}
  seek i (Buffer _ b) = Buffer j b
    where
      l = Seq.length b
      j = case i `mod` l of
            x | x < 0 -> l + x
            x         -> x

size :: Buffer a -> Int
size = Seq.length . getBuffer

{-# INLINE right #-}
right :: Buffer a -> Buffer a
right = shift 1

{-# INLINE left #-}
left :: Buffer a -> Buffer a
left = shift (-1)

{-# INLINE focus #-}
focus :: Buffer a -> a
focus = Seq.index <$> getBuffer <*> bufIdx

singleton :: a -> Buffer a
singleton a = Buffer 0 (pure a)

fromList :: [a] -> Buffer a
fromList xs = Buffer 0 (Seq.fromList xs)

rewind :: Buffer a -> Buffer a
rewind b = b { bufIdx = 0 }

{-# INLINE shift #-}
shift :: Int -> Buffer a -> Buffer a
shift n = seeks (+ n)

{-# INLINE insertR #-}
insertR :: a -> Buffer a -> Buffer a
insertR a (Buffer i b) = Buffer i (Seq.insertAt (succ i) a b)

{-# INLINE insertL #-}
insertL :: a -> Buffer a -> Buffer a
insertL a (Buffer i b) = Buffer (succ i) (Seq.insertAt i a b)

toList :: Buffer a -> [a]
toList = Foldable.toList . getBuffer

splitAt :: Int -> Buffer a -> (Buffer a,Buffer a)
splitAt len (Buffer i b) = (Buffer 0 (as <> as'), Buffer 0 (bs <> bs'))
  where
    (lefts, rights) = Seq.splitAt i b
    (as,bs) = Seq.splitAt len rights
    holdovers = len - Seq.length as
    (as',bs') = case holdovers of
                  0 -> (Seq.empty, lefts)
                  n -> Seq.splitAt n lefts

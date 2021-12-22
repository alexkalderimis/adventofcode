{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Elves.Array.CursorCycling (
  Cursor(toArray),
  cursor
  ) where

import           Control.Comonad
import           Control.Comonad.Store
import qualified Data.Array as Array
import           Data.Array (Array, Ix)
import           Data.Hashable

import Elves.Array.Wrapping

data Cursor i a = C { _index :: !i, toArray :: !(Array i a) }
  deriving (Show, Eq)

instance (Wrapping i, Hashable i, Hashable a) => Hashable (Cursor i a) where
  hashWithSalt s c = hashWithSalt s (pos c, Array.assocs (toArray c))

instance Ix i => Functor (Cursor i) where
  fmap f c = let a = toArray c
              in c { toArray = Array.listArray (Array.bounds a) (f <$> Array.elems a) }

instance Wrapping i => Comonad (Cursor i) where
  extract (C i a) = a Array.! wrap (Array.bounds a) i
  extend f c = let bs = Array.bounds (toArray c)
                   a' = Array.listArray bs [ f (seek i c) | i <- Array.range bs ]
                in c { toArray = a' }

instance Wrapping i => ComonadStore i (Cursor i) where
  pos = _index
  peek s = extract . seek s
  seek s c = c { _index = s }
  seeks f c = c { _index = f (_index c) }

cursor :: Wrapping i => Array i a -> Cursor i a
cursor a = c
  where
    bs = Array.bounds a
    c = C (head $ Array.range bs) a
    f i = peek (wrap bs i) c

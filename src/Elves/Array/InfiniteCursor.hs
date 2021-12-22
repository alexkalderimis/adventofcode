{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Elves.Array.InfiniteCursor (
  Cursor(toArray),
  cursor
  ) where

import           Control.Comonad
import           Control.Comonad.Store
import qualified Data.Array as Array
import           Data.Array (Array, Ix)
import           Data.Hashable

data Cursor i a = C { _index :: !i, _default :: !a, toArray :: !(Array i a) }
  deriving (Show, Eq)

instance (Hashable i, Hashable a, Ix i) => Hashable (Cursor i a) where
  hashWithSalt s c = hashWithSalt s (pos c, _default c, Array.assocs (toArray c))

instance Ix i => Functor (Cursor i) where
  fmap f c = let a = toArray c
             in c { _default = f (_default c)
                  , toArray = Array.listArray (Array.bounds a) (f <$> Array.elems a)
                  }

instance Ix i => Comonad (Cursor i) where
  extract c = if Array.inRange (Array.bounds $ toArray c) (pos c)
                       then toArray c Array.! pos c
                       else _default c

  extend f c = let bs = Array.bounds (toArray c)
                   a' = Array.listArray bs [ f (seek i c) | i <- Array.range bs ]
                in c { toArray = a', _default = f (fmap (pure $ _default c) c) }

instance Ix i => ComonadStore i (Cursor i) where
  pos = _index
  peek s = extract . seek s
  seek s c = c { _index = s }
  seeks f c = c { _index = f (_index c) }

cursor :: Ix i => a -> Array i a -> Cursor i a
cursor x a = C { _index = head (Array.range bs), _default = x, toArray = a }
  where
    bs = Array.bounds a

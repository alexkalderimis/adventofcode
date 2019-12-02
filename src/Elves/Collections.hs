-- make it easier to swap out different collection types
module Elves.Collections where

import Control.Arrow
import qualified Data.Array as A
import Data.Array (Array)
import qualified Data.List as L
import qualified Data.Vector as V
import           Data.Vector (Vector)

class Zips f where
  zipWith :: (a -> b -> c) -> f a -> f b -> f c

instance Zips [] where
  zipWith = L.zipWith

instance Zips Vector where
  zipWith = V.zipWith

instance (A.Ix ix) => Zips (Array ix) where
  zipWith f a b = A.listArray bs [f (a A.! i) (b A.! i) | i <- A.range bs]
    where
      ab = A.bounds a
      bb = A.bounds b
      bs = (max (fst ab) *** min (snd ab)) bb

class Cons f where
  cons :: a -> f a -> f a

instance Cons [] where
  cons = (:)

instance Cons Vector where
  cons = V.cons


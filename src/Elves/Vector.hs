module Elves.Vector (sortOn, groupOn) where

import qualified Data.Vector.Algorithms.Intro as VA
import qualified Data.Vector             as V
import           Data.Vector             (Vector)
import           Data.Ord

sortOn :: Ord b => (a -> b) -> Vector a -> Vector a
sortOn f = V.modify (VA.sortBy (comparing f))

groupOn :: Eq b => (a -> b) -> Vector a -> [Vector a]
groupOn f xs =
    switchL []
    (\h t ->
      let fh = f h
      in case V.findIndex ((fh /=) . f) t of
         Nothing -> [xs]
         Just n -> V.unsafeTake (n + 1) xs : groupOn f (V.unsafeDrop (n + 1) xs))
    xs
  where
    switchL n j v =
      if null v
        then n
        else j (V.unsafeHead v) (V.unsafeTail v)


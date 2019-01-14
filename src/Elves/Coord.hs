{-# LANGUAGE RankNTypes        #-}

module Elves.Coord where

import qualified Data.List as L
import           Control.Lens              hiding (contains, index)

type Accessor a b = ReifiedLens a a b b

class Coord a where
  dimensions :: [Accessor a Int]

instance Coord Int where
  dimensions = [Lens (lens id (pure id))]

intLens :: (Integral a) => Lens' s a -> Accessor s Int
intLens l = Lens $ lens (fromIntegral . view l) (\s i -> set l (fromIntegral i) s)

instance (Integral a, Integral b) => Coord (a,b) where
  dimensions = [intLens _1, intLens _2]

instance (Integral a, Integral b, Integral c) => Coord (a,b,c) where
  dimensions = [ intLens _1
               , intLens _2
               , intLens _3
               ]

instance (Integral a, Integral b, Integral c, Integral d) => Coord (a,b,c,d) where
  dimensions = [ intLens _1
               , intLens _2
               , intLens _3
               , intLens _4
               ]

manhattan :: Coord i => i -> i -> Int
manhattan a b = sum [abs (view (runLens l) a - view (runLens l) b) | l <- dimensions]

translate :: Coord i => i -> i -> i
translate delta pos = L.foldl' (\c d -> c & runLens d %~ (+ view (runLens d) delta)) pos dimensions

invert :: Coord i => i -> i
invert c = L.foldl' (\c d -> c & runLens d %~ negate) c dimensions

scale :: Coord i => Int -> i -> i
scale n c = L.foldl' (\p d -> p & runLens d %~ (* n)) c dimensions


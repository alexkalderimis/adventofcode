{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Elves.Matrix where

import qualified Data.List.Extra as L
import Data.Coerce
import qualified Data.Array as Array
import           Data.Array (Array)

import Elves.StrictGrid

-- simple array based matrices. We could use Repa for all of this, but the aim
-- here is to keep stuff as simple as possible, and reimplement some basic stuff
-- note: indices are 0-based
newtype Matrix a = Matrix { getMatrix :: Array Coord a } deriving (Eq, Functor)

instance Show a => Show (Matrix a) where
  show m = let (Coord (Row h) (Col w)) = dimensions m
               cells = show <$> Array.elems (getMatrix m)
               cellWidth = maximum (length <$> cells)
               justified = [ replicate (cellWidth - length c) ' ' <> c | c <- cells ]
               row cs = mconcat ["| ", L.intercalate ", " cs, " |"]
               lines = mconcat [show h, "x", show w, ":"] : fmap row (L.chunksOf w justified)
            in unlines lines

matrix :: Row -> Col -> [a] -> Matrix a
matrix rows cols = Matrix . Array.listArray (origin, (Coord (rows - 1) (cols - 1)))

identity :: Num a => Int -> Matrix a
identity i = matrix (Row i) (Col i) [if a == b then 1 else 0
                                    | (a, b) <- Array.range ((1, 1), (i, i))
                                    ]

dimensions :: Matrix a -> Coord
dimensions (Matrix a) = let (lb, ub) = Array.bounds a
                         in Coord ((row ub - row lb) + 1) ((col ub - col lb) + 1)

-- naive matrix multiplication
--
dotp :: Num a
     => Matrix a -- m x n
     -> Matrix a -- n x p
     -> Matrix a -- m x p
dotp a b | col (dimensions a) /= coerce (row (dimensions b)) = error "cols a /= rows b"
dotp a b = let (Coord m n) = dimensions a
               (Coord _ p) = dimensions b
               xs = [0 .. getCol n - 1]
               bs = (Coord 0 0, Coord (m - 1) (p - 1))
               value c = sum [ at (row c) (Col x) a * at (Row x) (col c) b | x <- xs ]
           in matrix m p (value <$> Array.range bs)
                
{-# INLINE at #-}
at :: Row -> Col -> Matrix a -> a
at r c m = getMatrix m Array.! (Coord r c)

{-# INLINE update #-}
update :: Row -> Col -> a -> Matrix a -> Matrix a
update r c x (Matrix a) = Matrix (a Array.// [(Coord r c, x)])

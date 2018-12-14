{-# LANGUAGE DeriveFunctor #-}

-- cycling list zippers
--
-- Use these for list manipulation where you need contant time
-- appends and link-listy kinds of features.
--
-- Not always the most performant options, especially when lots
-- of mutation is going on, or where random access is required.
module Elves.Zipper where

import qualified Data.List.NonEmpty as NE
import           Data.List.NonEmpty (NonEmpty(..))

data Zipper a = Zipper
  { idx    :: Int
  , zlen   :: Int
  , lefts  :: [a]
  , focus  :: a
  , rights :: [a]
  } deriving (Show, Eq, Functor)

singleton :: a -> Zipper a
singleton a = Zipper 0 1 [] a []

left :: Zipper a -> Zipper a
left (Zipper i n [] a [])         = Zipper i n [] a []
left (Zipper i n [] a rs)         = left (Zipper i n (reverse rs) a [])
left (Zipper i n (new:ls) old rs) = Zipper (pred i) n ls new (old:rs)

right :: Zipper a -> Zipper a
right (Zipper i _ [] a [])         = Zipper i 1 [] a []
right (Zipper i n ls a [])         = right (Zipper i n [] a (reverse ls))
right (Zipper i n ls old (new:rs)) = Zipper (succ i) n (old:ls) new rs

-- insert one element to the right of the focus
insertR :: a -> Zipper a -> Zipper a
insertR elem (Zipper i n ls a rs) = Zipper i (n + 1) ls a (elem:rs)

-- insert one element to the left of the focus
insertL :: a -> Zipper a -> Zipper a
insertL elem (Zipper i n ls a rs) = Zipper (i + 1) (n + 1) (elem:ls) a rs

shiftToEnd :: Zipper a -> Zipper a
shiftToEnd z = shiftTo (zlen z - 1) z

rewind :: Zipper a -> Zipper a
rewind = shiftTo 0

shift :: Int -> Zipper a -> Zipper a
shift 0 z = z
shift n z
  | n < 0     = shift (succ n) (left z)
  | otherwise = shift (pred n) (right z)

shiftTo :: Int -> Zipper a -> Zipper a
shiftTo i z = shift (i - idx z) z

toList :: Zipper a -> [a]
toList z = focus z : rights z ++ reverse (lefts z)

fromList :: [a] -> Maybe (Zipper a)
fromList []     = Nothing
fromList (a:as) = pure (Zipper 0 (1 + length as) [] a as)

fromNonEmpty :: NE.NonEmpty a -> Zipper a
fromNonEmpty (a :| as) = Zipper 0 (1 + length as) [] a as

indexed :: Zipper a -> Zipper (Int, a)
indexed z = let i = idx z in z { lefts = zip (iterate pred (pred i)) (lefts z)
                               , focus = (i, focus z)
                               , rights = zip (iterate succ (succ i)) (rights z)
                               }

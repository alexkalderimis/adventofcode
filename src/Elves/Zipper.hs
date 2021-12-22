{-# LANGUAGE DeriveFunctor #-}

-- cycling list zippers
--
-- Use these for list manipulation where you need contant time
-- appends and link-listy kinds of features.
--
-- Not always the most performant options, especially when lots
-- of mutation is going on, or where random access is required.
module Elves.Zipper where

import Test.QuickCheck.Arbitrary (Arbitrary (arbitrary))
import           Control.Comonad
import           Data.Maybe (catMaybes, isJust)
import qualified Data.List.NonEmpty as NE
import           Data.List.NonEmpty (NonEmpty(..))

data Zipper a = Zipper
  { idx    :: !Int
  , zlen   :: !Int
  , lefts  :: [a]
  , focus  :: !a
  , rights :: [a]
  } deriving (Show, Eq, Functor)

instance Comonad Zipper where
  extract = focus
  duplicate z = let shift dir = catMaybes . takeWhile isJust . tail . iterate (>>= dir) . Just
                 in z { lefts = shift left z, focus = z, rights = shift right z }

instance Arbitrary a => Arbitrary (Zipper a) where
  arbitrary = do
    ls <- arbitrary
    a  <- arbitrary
    rs <- arbitrary
    pure $ Zipper (length ls) (1 + length ls + length rs) ls a rs

singleton :: a -> Zipper a
singleton a = Zipper 0 1 [] a []

-- non-cycling navigation

right :: Zipper a -> Maybe (Zipper a)
right (Zipper os zl lhs old (new:rhs)) = pure $ Zipper (succ os) zl (old:lhs) new rhs
right _ = Nothing

left :: Zipper a -> Maybe (Zipper a)
left (Zipper os zl (new:lhs) old rhs) = pure $ Zipper (pred os) zl lhs new (old:rhs)
left _                             = Nothing

-- cycling navigation

cright :: Zipper a -> Zipper a
cright z = let (ls,a,rs) = go (lefts z) (focus z) (rights z)
           in z { lefts = ls, rights = rs, focus = a, idx = idx' }
  where 
    idx' = (succ $ idx z) `mod` (zlen z)
    go [] a []         = ([],a,[])
    go ls a []         = go [] a (reverse ls)
    go ls old (new:rs) = (old:ls,new,rs)

cleft :: Zipper a -> Zipper a
cleft z = let (ls,a,rs) = go (lefts z) (focus z) (rights z)
          in z { lefts = ls, rights = rs, focus = a, idx = idx' }
  where 
    idx' = if idx z == 0 then zlen z - 1 else pred (idx z)
    go [] a []         = ([],a,[])
    go [] a rs         = go (reverse rs) a []
    go (new:ls) old rs = (ls,new,old:rs)

rewind :: Zipper a -> Zipper a
rewind = seek 0

fromNonEmpty :: NonEmpty a -> Zipper a
fromNonEmpty (a :| as) = Zipper 0 (1 + length as) [] a as

fromList :: [a] -> Maybe (Zipper a)
fromList []     = Nothing
fromList (a:as) = pure $ fromNonEmpty (a :| as)

-- generates a view from the focus forward.
-- If you want the list to start from index 0, then please be kind, rewind!
toList :: Zipper a -> [a]
toList z = focus z : rights z ++ reverse (lefts z)

-- we could have infinite zippers, but growing them is
-- slightly better as they stay showable.
grow :: a -> Zipper a -> Zipper a
grow a (Zipper os ln ls c rs) = Zipper os (ln + 4) (ls ++ replicate 2 a) c (rs ++ replicate 2 a)

indexed :: Zipper a -> Zipper (Int, a)
indexed z = let i = idx z in z { lefts = zip (iterate pred (pred i)) (lefts z)
                               , focus = (i, focus z)
                               , rights = zip (iterate succ (succ i)) (rights z)
                               }

-- insert one element to the right of the focus
insertR :: a -> Zipper a -> Zipper a
insertR elem (Zipper i n ls a rs) = Zipper i (n + 1) ls a (elem:rs)

-- insert one element to the left of the focus
insertL :: a -> Zipper a -> Zipper a
insertL elem (Zipper i n ls a rs) = Zipper (i + 1) (n + 1) (elem:ls) a rs

shift :: Int -> Zipper a -> Zipper a
shift n z 
  | mod n (zlen z) == 0 = z
  | n < 0     = shift (succ n) (cleft z)
  | otherwise = shift (pred n) (cright z)

seek :: Int -> Zipper a -> Zipper a
seek i z = shift (i - idx z) z

seekEnd :: Zipper a -> Zipper a
seekEnd z = maybe z seekEnd (right z)

seekStart :: Zipper a -> Zipper a
seekStart z = maybe z seekStart (left z)


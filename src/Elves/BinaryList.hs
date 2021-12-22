{-# LANGUAGE DeriveFunctor #-}

module Elves.BinaryList where

import Prelude hiding (length)
import qualified Data.Ix as Ix

data List a
  = Single a
  | Pair !Int !Int (List a) (List a)
  deriving (Eq, Functor)

instance Show a => Show (List a) where
  show (Single x) = show x
  show (Pair _ _ a b) = mconcat ["[", show a, ",", show b, "]"]

data ListZ a = Zipper
  { depth :: Int
  , offset :: Int
  , leftSib  :: Maybe (List a)
  , rightSib :: Maybe (List a)
  , above :: Maybe (ListZ a)
  , focus :: List a
  } deriving (Show, Eq, Functor)

transform :: (a -> a) -> ListZ a -> ListZ a
transform f z = z { focus = fmap f (focus z) }

replace :: List a -> ListZ a -> ListZ a
replace foc z = z { focus = foc }

length :: List a -> Int
length (Single a) = 1
length (Pair n m _ _) = n + m

maxDepth :: List a -> Int
maxDepth (Single a) = 1
maxDepth (Pair _ _ a b) = max (maxDepth a) (maxDepth b)

single :: a -> List a
single = Single

pair :: List a -> List a -> List a
pair a b = Pair (length a) (length b) a b

indices :: ListZ a -> (Int, Int)
indices z = (offset z, offset z + length (focus z) - 1)

locations :: ListZ a -> [ListZ a]
locations = go . goTo 0 . top
  where
    go Nothing = []
    go (Just z) = z : go (rightNeighbour z)

rezip :: ListZ a -> List a
rezip z =
  let inner = rezipLevel z
  in case above z of
    Nothing -> inner
    Just p -> rezip (p { focus = inner })

rezipLevel :: ListZ a -> List a
rezipLevel z = case (leftSib z, rightSib z) of
  (Nothing, Nothing) -> focus z
  (Just lh, Nothing) -> pair lh (focus z)
  (Nothing, Just rh) -> pair (focus z) rh
  _ -> error "impossible"

listz :: List a -> ListZ a
listz (Single x)       = Zipper 0 0 Nothing Nothing Nothing (Single x)
listz (Pair n m lh rh) = Zipper 0 0 Nothing (Just rh) Nothing lh

leftNeighbour :: ListZ a -> Maybe (ListZ a)
leftNeighbour z = goTo (offset z - 1) z

rightNeighbour :: ListZ a -> Maybe (ListZ a)
rightNeighbour z = goTo (snd (indices z) + 1) z

goTo :: Int -> ListZ a -> Maybe (ListZ a)
goTo i _ | i < 0                    = Nothing
goTo i z | i < offset z             = left z >>= goTo i
goTo i z | Ix.inRange (indices z) i = case focus z of Single{} -> pure z
                                                      _ -> down z >>= goTo i
goTo i z                            = right z >>= goTo i

left :: ListZ a -> Maybe (ListZ a)
left z = case leftSib z of
  Nothing -> up z >>= left
  Just xs -> Just $ z { offset = offset z - length xs
                      , focus = xs
                      , leftSib = Nothing
                      , rightSib = Just (focus z)
                      }

right :: ListZ a -> Maybe (ListZ a)
right z = case rightSib z of
  Nothing -> up z >>= right
  Just xs -> Just $ z { offset = offset z + length (focus z)
                      , focus = xs
                      , leftSib = Just (focus z)
                      , rightSib = Nothing
                      }

leaf :: ListZ a -> Bool
leaf z = case focus z of Single{} -> True
                         _ -> False

up :: ListZ a -> Maybe (ListZ a)
up z = fmap (\p -> p { focus = rezipLevel z }) (above z)

top :: ListZ a -> ListZ a
top z = maybe z top (up z)

down :: ListZ a -> Maybe (ListZ a)
down z = case focus z of
  Single _ -> Nothing
  (Pair n m lh rh) -> pure $ Zipper (depth z + 1) (offset z)
                             Nothing
                             (Just rh)
                             (Just z)
                             lh

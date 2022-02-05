module Elves.Ord where

newtype NullsLast a = NullsLast { nullsLast :: Maybe a }
  deriving (Show, Eq)

instance Ord a => Ord (NullsLast a) where
  (NullsLast Nothing) <= _ = False
  _ <= (NullsLast Nothing) = True
  (NullsLast (Just a)) <= (NullsLast (Just b)) = a <= b

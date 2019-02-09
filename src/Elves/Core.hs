module Elves.Core where

data Where a b = Neither | Diff a b | InLeft a | InRight b
  deriving (Show, Eq)

class ShowDiff a where
  showDiff :: a -> a -> Maybe String


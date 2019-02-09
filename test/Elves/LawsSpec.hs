module Elves.LawsSpec where

import Data.Functor.Identity
import           Test.Hspec
import           Test.QuickCheck          hiding (within)
import           Data.Functor.Compose
import qualified Test.QuickCheck          as QC
import qualified Test.QuickCheck.Property as QCP

import qualified Debug.Trace as Debug

import Elves.Core

type Comparator a = (a -> a -> Property)
type Cast a = (a -> a)

eq :: (ShowDiff a) => Comparator a
eq a b = let diff = showDiff a b
          in QCP.counterexample (maybe "No Difference" ("Difference: " ++) diff)
                                (diff == Nothing)

cast :: Cast a
cast = id

monoid :: (Arbitrary a, Monoid a, Show a) => Comparator a -> SpecWith ()
monoid eq = describe "Monoid" $ parallel $ do
  let t_o = 100 * 10000
  specify "mempty is right identity" $ property $ \t -> QC.within t_o $
    (t <> mempty) `eq` t
  specify "mempty is left identity" $ property $ \t -> QC.within t_o $
    (mempty <> t) `eq` t
  specify "semigroup law" $ withMaxSuccess 75 $ \a b c -> QC.within t_o $ do
    (a <> (b <> c)) `eq` ((a <> b) <> c)
  specify "mconcat is fold of <>" $ withMaxSuccess 20 $ \ts -> QC.within t_o $
    mconcat ts `eq` foldr (<>) mempty ts

traversable :: (Traversable t, Arbitrary (t a), Show (t a), Eq (t a)) => Cast (t a) -> SpecWith ()
traversable cast = describe "Traversable" $ do
    let t = maybe (Left "urk") Right
        f = Just 
        g a = [a]
    specify "naturality" $ property $ \rt ->
      (t . traverse f $ rt) === (traverse (t . f) (cast rt))
    specify "identity" $ property $ \rt ->
      (traverse Identity $ rt) === (Identity (cast rt))
    specify "composition" $ property $ \rt ->
      let lhs = traverse (Compose . fmap g . f) (cast rt)
          rhs = Compose . fmap (traverse g) . traverse f $ rt
       in lhs === rhs

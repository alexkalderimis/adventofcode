module Elves.LawsSpec where

import Data.Functor.Identity
import           Test.Hspec
import           Test.QuickCheck          hiding (within)
import           Data.Functor.Compose
import qualified Test.QuickCheck          as QC
import qualified Test.QuickCheck.Property as QCP

type Comparator a = (a -> a -> Property)
type Cast a = (a -> a)

eq :: (Eq a, Show a) => Comparator a
eq = (===)

comparesEq :: (Ord a, Show a) => Comparator a
comparesEq a b = QCP.counterexample (unwords [show a, "/=", show b])
                                    (compare a b == EQ)

cast :: Cast a
cast = id

monoid :: (Arbitrary a, Monoid a, Show a) => Comparator a -> SpecWith ()
monoid eq = describe "Monoid" $ do
  specify "mempty is right identity" $ QC.within 10000 $ \t ->
    (t <> mempty) `eq` t
  specify "mempty is left identity" $ QC.within 10000 $ \t ->
    (mempty <> t) `eq` t
  specify "semigroup law" $ QC.within 10000 $ \a b c ->
    (a <> (b <> c)) `eq` ((a <> b) <> c)
  specify "mconcat is fold of <>" $ QC.within 10000 $ \ts ->
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

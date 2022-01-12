{-# LANGUAGE NumericUnderscores #-}
module Elves.LawsSpec where

import Data.Functor.Identity
import           Test.Hspec
import           Test.QuickCheck          hiding (within)
import           Data.Functor.Compose
import qualified Test.QuickCheck          as QC
import qualified Test.QuickCheck.Property as QCP
import Data.Maybe

import qualified Debug.Trace as Debug

import Elves.Core
import Support

eq :: (ShowDiff a) => Comparator a
eq a b = let diff = showDiff a b
          in QCP.counterexample (maybe "No Difference" ("Difference: " ++) diff)
                                (isNothing diff)

equality :: (Arbitrary a, Eq a, Show a, Show b, Eq b)
         => Cast a -> (a -> b) -> (a -> a) -> (a -> a) -> SpecWith ()
equality cast f preserve purturb = describe "Eq" $ do
  specify "symmetry" . propertyFor cast $ \a b -> (a == b) === (b == a)
  specify "reflexivity" . propertyFor cast $ \x -> x === x
  specify "negation" . propertyFor cast $ \a b ->
    (a /= b) === not (b == a)
-- These tests are too hard to test correctly - it is just really
-- unlikely that we can generate enough examples where the implication is met
-- specify "substitutivity" $ propertyFor cast $ \a b ->
--   (a == b) ==> (f a === f b)
-- specify "transitivity-eq" . propertyFor cast $ \a b c ->
--   (a == b && b == c) ==> a === c
-- specify "transitivity-neq" . propertyFor cast $ \a b c ->
--   (a == b && b /= c) ==> a =/= c

monoid :: (Arbitrary a, Monoid a, Show a) => Comparator a -> SpecWith ()
monoid eq = describe "Monoid" . parallel $ do
  let t_o = 1_000_000
  let is a b = QC.within t_o (a `eq` b)

  specify "mempty is right identity" . withMaxSuccess 20 $ \t -> (t <> mempty) `is` t
  specify "mempty is left identity" . withMaxSuccess 20 $ \t -> (mempty <> t) `is` t
  specify "semigroup law" . withMaxSuccess 20 $ \a b c -> (a <> (b <> c)) `is` ((a <> b) <> c)
  specify "mconcat is fold of <>" . withMaxSuccess 20 $ \ts -> mconcat ts `is` foldr (<>) mempty ts

traversable :: (Traversable t, Arbitrary (t a), Show (t a), Eq (t a)) => Cast (t a) -> SpecWith ()
traversable cast = describe "Traversable" $ do
    let t = maybe (Left "urk") Right
        f = Just 
        g a = [a]
    specify "naturality" . property $ \rt ->
      (t . traverse f $ rt) === traverse (t . f) (cast rt)
    specify "identity" . property $ \rt ->
      traverse Identity rt === Identity (cast rt)
    specify "composition" . property $ \rt ->
      let lhs = traverse (Compose . fmap g . f) (cast rt)
          rhs = Compose . fmap (traverse g) . traverse f $ rt
       in lhs === rhs

module Support where

import qualified Data.List.Extra as L
import Control.Monad

import           Test.Hspec.Expectations
import           Test.QuickCheck

type Comparator a = (a -> a -> Property)
type Cast a = (a -> a)

{-# INLINE propertyFor #-}
propertyFor :: (Show a, Arbitrary a, Testable prop) => Cast a -> (a -> prop) -> Property
propertyFor cast f = property (f . cast)

{-# INLINE cast #-}
cast :: Cast a
cast = id

shouldContainExactly :: (Ord a, Eq a, Show a) => [a] -> [a] -> Expectation
shouldContainExactly xs ys = L.sort xs `shouldBe` L.sort ys

shouldAll :: (HasCallStack, Foldable collection)
                 => collection a -> (a -> Expectation) -> Expectation
shouldAll = forM_

andAlso :: (a -> Expectation) -> (a -> Expectation) -> a -> Expectation
andAlso f g a = f a >> g a

module Elves.CountMapSpec (spec) where

import Elves.CountMap

import Test.Hspec
import Test.QuickCheck

import           Data.Monoid
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as M

spec :: Spec
spec = describe "Elves.CountMap" $ do
  specify "sum (keys m) == length xs" $ property $ \(ASCIIString xs) ->
    let m = fromList xs
     in (fromIntegral . getSum . sum . M.elems $ countMap m) === length xs
  specify "count a E A > 0" $ property $ \a (ASCIIString xs) ->
    let m = fromList (a:xs)
     in countOf a m > 0
  specify "countOf a m >= occurrences of a in A" $ property $ \(Positive n) a (ASCIIString xs) ->
    let m = fromList $ replicate n a ++ xs
     in countOf a m >= fromIntegral n
  specify "countOf a m < countOf a (count a m)" $ property $ \a (ASCIIString xs) ->
    let m = fromList xs
     in countOf a m === countOf a (count a m) - 1
  specify "fromList xs <> fromList ys == fromList (xs ++ ys)" $ property $ \(ASCIIString xs) (ASCIIString ys) ->
    (fromList xs <> fromList ys) === fromList (xs ++ ys)


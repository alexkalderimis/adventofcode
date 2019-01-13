{-# LANGUAGE OverloadedStrings #-}

module Elves.KnotSpec (spec) where

import qualified Data.List as L

import Test.Hspec
import Test.QuickCheck

import Elves.Knot
import qualified Elves.CircularBuffer as B

spec :: Spec
spec = describe "Elves.Knot" $ do
  let buff i = B.shift i . B.fromList
  describe "pinch" $ do
    describe "Day 10 examples" $ do
      let b = B.fromList "01234"
          lengths = [3,4,1,5]
          states = L.scanl (flip (uncurry pinch)) b (zip lengths [0 ..])
      it "generates the right states" $ do
        states `shouldBe` [buff 0 "01234"
                          ,buff 3 "21034"
                          ,buff 3 "43012"
                          ,buff 1 "43012"
                          ,buff 4 "34210"
                          ]
  describe "asciiHash" $ do
    it "can hash the empty string" $ do
      hash "" `shouldBe` "a2582a3a0e66e6e86e3812dcb672a272"
    it "can hash AoC 2017" $ do
      hash "AoC 2017" `shouldBe` "33efeb34ea91902bb2f59c9920caa6cd"
    it "can hash 1,2,3" $ do
      hash "1,2,3" `shouldBe` "3efbe78a8d82f29979031a4aa0b16a9d"
    it "can hash 1,2,4" $ do
      hash "1,2,4" `shouldBe` "63960835bcdc130f0b66d7ff4f6a5a8e"


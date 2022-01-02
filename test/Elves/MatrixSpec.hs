{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Elves.MatrixSpec (spec) where

import Data.Maybe
import Data.Coerce
import qualified Data.List as L
import Control.Arrow ((&&&))
import Test.Hspec
import Test.QuickCheck
import Control.Lens (over, view)

import Elves.Matrix
import Elves.StrictGrid

spec :: Spec
spec = describe "Elves.Matrix" $ do
  describe "dotp" $ do
    it "performs matrix multiplication" $ do
      let a = matrix (Row 4) (Col 2) [ 1, 2
                                     , 3, 4
                                     , 5, 6
                                     , 7, 8
                                     ]
      let b = matrix (Row 2) (Col 4) [ 2, 3, 5, 7
                                     , 11, 13, 17, 19
                                     ]

          c = dotp a b
      at 0 1 c `shouldBe` (at 0 0 a * at 0 1 b) + (at 0 1 a * at 1 1 b)
      at 2 2 c `shouldBe` (at 2 0 a * at 0 2 b) + (at 2 1 a * at 1 2 b)

  describe "identity" $ do
    it "is a diagonal of ones" $ do
      identity 4 `shouldBe` matrix (Row 4) (Col 4) [ 1, 0, 0, 0
                                                   , 0, 1, 0, 0
                                                   , 0, 0, 1, 0
                                                   , 0, 0, 0, 1
                                                   ]
    it "is the identity of dotp" $
      forAll arbitrary              $ \(Positive rows) ->
      forAll arbitrary              $ \(Positive cols) ->
      forAll (vector (rows * cols)) $ \elems ->
        let m = matrix (Row rows) (Col cols) (elems :: [Int])
         in dotp (identity rows) m `shouldBe` m




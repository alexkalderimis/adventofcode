{-# LANGUAGE OverloadedStrings #-}

module Elves.Math.SymbolicSpec where

import Control.Applicative
import Test.Hspec
import Test.QuickCheck

import qualified Data.HashSet as S

import Elves.Math.Symbolic
import Elves.Math.Expression
import Elves.Math.Simplify
import Elves.Math.Differentiation

spec = describe "Elves.Math.Symbolic" $ do
  let x = Var "x"
      y = var "y"
      dxx = fullSimplify . dx "x"
  describe "(3 * (3x^2 - 5))^2 * 6x" $ do
    let e = (3 * ((squareOf (3 * x) - 5) `raisedTo` 2)) * (6 * x)
    it "simplifies correctly" $ do
      fullSimplify e `shouldBe` (18 * (x * squareOf (squareOf (3 * x) - 5)))
  specify "simplify does not affect evaluation" . property $ \expr (NonEmpty vs) -> within 100000 $
      let expr' = fullSimplify expr
          vars = S.toList (variables expr)
          bindings = zip vars (cycle vs)
       in eval bindings expr' ==~ (eval bindings expr :: Either String Double)
  describe "simplify" $ do
    describe "5(2 + x)" $ do
      let e = 5 * (2 + x)
      it "simplifies to 5x + 10" $ do
        fullSimplify e `shouldBe` ((5 * x) + 10)
    describe "3(5x+4)" $ do
      let e = 3 * (5 * x + 4)
      it "simplifies to 15x + 12" $ do
        fullSimplify e `shouldBe` ((15 * x) + 12)
    describe "(x^2)^2" $ do
      let e = (x ** 2) ** 2
      it "simplifies to x^4" $ do
        fullSimplify e `shouldBe` (x ** 4)
    describe "5(2+x) + 3(5x+4) - (x^2)^2" $ do
      let e = sum [5 * (2 + x)
                  ,3 * (5 * x + 4)
                  ,negate $ (x ** 2) ** 2
                  ]
      it "simplifies to -x^4 + 20x + 22" $ do
        fullSimplify e `shouldBe` (negate (x ** 4) + ((20 * x) + 22))
    describe "(2x-y)(3y-2x)" $ do
      let e = product [2 * x - y, 3 * y - 2 * x]
      it "simplifies to -4x^2 + 8xy - 3y^2" . within 10000 $ do
        let e' = sum [-4 * (x ** 2)
                     , 8 * x * y
                     ,-3 * (y ** 2)
                     ]
        fullSimplify e `shouldBe` fullSimplify e'
    describe "(2x-y)(3y-2x)(6x-2y)(3x-y)" $ do
      let e = product [2 * x - y, 3 * y - 2 * x, 6 * x - 2 * y, 3 * x - y]
      it "simplifies to -72(x^4) + 192(x^3)y - 158(x^2)(y^2) + 52x(y^3) -6(y^4)" $ do
        fullSimplify e `shouldBe` fullSimplify (sum [-72 * (x ** 4) * (y ** 0)
                                                    ,192 * (x ** 3) * (y ** 1)
                                                    ,158 * (x ** 2) * (y ** 2)
                                                    , 52 * (x ** 1) * (y ** 3)
                                                    , -6 * (x ** 0) * (y ** 4)
                                                    ])

  describe "differentiation" $ do
    it "handles constants" . property $ \x ->
      dxx (Constant x) === Constant (0 :: Double)
    it "handles variables" . property $ \(V x) ->
      dx x (Var x) === Constant (1 :: Double)
    it "handles fixed values" . property $ \(V x) ->
      dxx (var x) === Constant (if x == "x" then 1 else 0 :: Double)
    it "handles x^2" $ do
      dxx (squareOf x) `shouldBe` (2 * x)
    it "handles x^3" $ do
      dxx (x ** 3) `shouldBe` (3 * squareOf x)
    it "handles sin" $ do
      dxx (sin x) `shouldBe` cos x
    it "handles 1/x" $ do
      dxx (1 / x) `shouldBe` negate (1 / squareOf x)
    it "handles sin(x) + x^2 (sum)" $ do
      dxx (sin x + squareOf x) `shouldBe` (cos x + 2 * x)
    it "handles sin(x) * x^2 (product)" $ do
      dxx (sin x * squareOf x) `shouldBe` ((sin x * (2 * x)) + (squareOf x * cos x))
    it "handles 2sin(x) (product with constant)" $ do
      dxx (2 * sin x) `shouldBe` (2 * cos x)
    it "handles sin(x^2) (composition)" $ do
      dxx (sin $ squareOf x) `shouldBe` (cos (squareOf x) * (2 * x))
    it "handles exponents of constants" $ do
      dxx (2 ** (3 * x + y)) `shouldBe` ((3 * log 2) * (2 ** (3 * x + y)))
    it "handles exponents of fixed variables" $ do
      dxx (y ** (x * 2)) `shouldBe` ((2 * (y ** (2 * x))) * log y)

  describe "df/dx (3x^2 - 5)^3" $ do
    let e = (3 * squareOf x - 5) ** 3
    it "equals 18x(3x^2 - 5)^2" $ do
      dxx e `shouldBe` (18 * (x * squareOf (3 * squareOf x - 5)))
  describe "sqrt x" $ do
    let e = squareRootOf x
    it "equals 1/2.x^(-1/2)" $ do
      dxx e `shouldBe` (0.5 * (x ** (-0.5)))
  describe "x2 + 1" $ do
    let e = squareOf x + 1
    it "equals 2x" $ do
      dxx e `shouldBe` (2 * x)

  describe "df/dx √(x^2 + 1)" $ do
    let e = squareRootOf $ squareOf x + 1
    it "equals ½(x^2 + 1)^-½ . 2x" $ do
      dxx e `shouldBe` (0.5 * (((squareOf x + 1) ** (-0.5)) * (2 * x)))

a ==~ b = counterexample (unwords [show a, "/=~", show b])
  $ case (a,b) of
      (Left err, Left err') -> err == err'
      _ -> either (pure False) id $ liftA2 kindaEqual a b

kindaEqual a b =  (isInfinite a && isInfinite b)
               || (isNaN a && isNaN b) -- shouldn't happen, but good to check 
               || abs (a - b) < 0.0000001

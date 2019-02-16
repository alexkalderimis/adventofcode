module Elves.Math.SymbolicSpec where

import Test.Hspec
import Test.QuickCheck
import Data.Text (Text, pack)

import Elves.Math.Symbolic

newtype Var = V Text deriving (Show, Eq)

instance Arbitrary Var where
  arbitrary = V . pack . pure <$> elements ['a' .. 'z']

spec = do
  let x = Var "x"
  describe "(3 * (3x^2 - 5))^2 * 6x" $ do
    let e = (3 * ((squareOf (3 * x) - 5) `raisedTo` 2)) * (6 * x)
    it "simplifies correctly" $ do
      fullSimplify e `shouldBe` (18 * x * squareOf (squareOf (3 * x) - 5))
  describe "differentiation" $ do
    it "handles constants" . property $ \x ->
      dx (Constant x) === Constant (0 :: Double)
    it "handles variables" . property $ \(V x) ->
      dx (Var x) === Constant (1 :: Double)
    it "handles x^2" $ do
      dx (squareOf x) `shouldBe` (2 * x)
    it "handles x^3" $ do
      dx (x ** 3) `shouldBe` (3 * squareOf x)
    it "handles sin" $ do
      dx (sin x) `shouldBe` cos x
    it "handles 1/x" $ do
      dx (1 / x) `shouldBe` negate (1 / squareOf x)
    it "handles sin(x) + x^2 (sum)" $ do
      dx (sin x + squareOf x) `shouldBe` (cos x + 2 * x)
    it "handles sin(x) * x^2 (product)" $ do
      dx (sin x * squareOf x) `shouldBe` ((sin x * (2 * x)) + (squareOf x * cos x))
    it "handles 2sin(x) (product with constant)" $ do
      dx (2 * sin x) `shouldBe` (2 * cos x)
    it "handles sin(x^2) (composition)" $ do
      dx (sin $ squareOf x) `shouldBe` (cos (squareOf x) * (2 * x))

  describe "df/dx (3x^2 - 5)^3" $ do
    let e = (3 * squareOf x - 5) ** 3
    it "equals 18x(3x^2 - 5)^2" $ do
      dx e `shouldBe` (18 * x * squareOf (3 * squareOf x - 5))
  describe "sqrt x" $ do
    let e = squareRootOf x
    it "equals 1/2.x^(-1/2)" $ do
      dx e `shouldBe` (0.5 * (x ** (-0.5)))
  describe "x2 + 1" $ do
    let e = squareOf x + 1
    it "equals 2x" $ do
      dx e `shouldBe` (2 * x)

  describe "df/dx √(x^2 + 1)" $ do
    let e = squareRootOf $ squareOf x + 1
    it "equals ½(x^2 + 1)^-½ . 2x" $ do
      dx e `shouldBe` (0.5 * (((squareOf x + 1) ** (-0.5)) * (2 * x)))


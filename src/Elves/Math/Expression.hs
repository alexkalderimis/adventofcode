module Elves.Math.Expression where

import           Data.HashSet    (HashSet)
import qualified Data.HashSet    as S
import           Data.Maybe
import           Data.Text       (Text, pack, unpack)

import           Test.QuickCheck hiding (Fixed, Fun)

type Variable = Text
type Set = HashSet

newtype Var = V Text deriving (Show, Eq)

instance Arbitrary Var where
  arbitrary = V . pack . pure <$> elements ['a' .. 'z']

data Expr a = Var Variable
             | NaN
             | Constant a
             | Fixed Variable
             | Mul    (Expr a) (Expr a)
             | Add    (Expr a) (Expr a)
             | Raise  (Expr a) (Expr a)
             | Divide (Expr a) (Expr a)
             | Application Fun (Expr a)
             deriving (Show, Ord, Eq)

instance Arbitrary a => Arbitrary (Expr a) where
  arbitrary = choose (1,25) >>= go
    where
      terminals :: [Int]
      terminals = [0 .. 3]
      allCons :: [Int]
      allCons = [0 .. 8]
      go n = do
        x <- if n > (0 :: Int)
                then elements allCons
                else elements terminals
        case x of
          0 -> do (V x) <- arbitrary
                  return (Var x)
          1 -> pure NaN
          2 -> Constant <$> arbitrary
          3 -> Fixed . pack . pure <$> elements ['A' .. 'Z']
          4 -> Mul         <$> go (n - 1) <*> go (n - 1)
          5 -> Add         <$> go (n - 1) <*> go (n - 1)
          6 -> Raise       <$> go (n - 1) <*> go (n - 1)
          7 -> Divide      <$> go (n - 1) <*> go (n - 1)
          8 -> Application <$> elements [minBound .. maxBound] <*> arbitrary
  shrink e = case e of
    (Mul a b)         -> [a,b]
    (Add a b)         -> [a,b]
    (Divide a b)      -> [a,b]
    (Raise a b)       -> [a,b]
    (Application f a) -> fmap (Application f) (shrink a)
    _                 -> []

instance (Num a) => Num (Expr a) where
  (+) = Add
  (*) = Mul
  a - b = a `Add` negate' b
  negate = negate'
  fromInteger = Constant . fromInteger
  abs = Application Absolute
  signum = Application SigNum

instance (Fractional a) => Fractional (Expr a) where
  (/) = Divide
  fromRational = Constant . fromRational

instance (Floating a) => Floating (Expr a) where
  pi = Constant pi
  exp = Application Exp
  log = Application Logn
  sqrt = squareRootOf
  (**) = Raise
  sin  = Application Sin
  cos  = Application Cos
  tan  = Application Tan
  asin = Application ASin
  acos = Application ACos
  atan = Application ATan
  sinh = Application SinH
  cosh = Application CosH
  tanh = Application TanH
  asinh = Application ASinH
  acosh = Application ACosH
  atanh = Application ATanH

data Fun = SigNum | Absolute
         | Sin | Cos | Tan
         | ASin | ACos | ATan
         | SinH | CosH | TanH
         | ASinH | ACosH | ATanH
         | Exp | Logn
  deriving (Show, Eq, Ord, Enum, Bounded)

var :: Variable -> Expr a
var = Var

variables :: Expr a -> Set Variable
variables (Var v)           = S.singleton v
variables (Fixed v)         = S.singleton v
variables (Add a b)         = variables a <> variables b
variables (Mul a b)         = variables a <> variables b
variables (Divide a b)      = variables a <> variables b
variables (Raise a b)       = variables a <> variables b
variables (Application _ a) = variables a
variables _                 = S.empty

bottomUp :: (Expr a -> Expr a) -> Expr a -> Expr a
bottomUp f e = f (applyChildren f e)
  where
    applyChildren f e = case e of
              Add    a b      -> bottomUp f a `Add`    bottomUp f b
              Mul    a b      -> bottomUp f a `Mul`    bottomUp f b
              Raise  a b      -> bottomUp f a `Raise`  bottomUp f b
              Divide a b      -> bottomUp f a `Divide` bottomUp f b
              Application g a -> Application g (bottomUp f a)
              _               -> e

negate' :: (Num a) => Expr a -> Expr a
negate' (Constant a)   = Constant (negate a)
negate' (a `Add` b)    = negate' a `Add` negate' b
negate' (a `Mul` b)    = negate' a `Mul` b
negate' (a `Divide` b) = negate' a `Divide` b
negate' e              = Constant (-1) `Mul` e

squareOf :: (Num a) => Expr a -> Expr a
squareOf x = x `raisedTo` 2

raisedTo :: (Num a) => Expr a -> a -> Expr a
raisedTo x y = x `Raise` Constant y

squareRootOf :: (Floating a) => Expr a -> Expr a
squareRootOf x = x `Raise` Constant 0.5

minus :: (Num a) => Expr a -> a -> Expr a
minus expr x = expr `Add` Constant (negate x)

mapVar :: (Variable -> Maybe (Expr a)) -> Expr a -> Expr a
mapVar f (Var x)            = fromMaybe (Var x) (f x)
mapVar f (Fixed x)          = fromMaybe (Fixed x) (f x)
mapVar f (a `Add` b)        = mapVar f a `Add` mapVar f b
mapVar f (a `Mul` b)        = mapVar f a `Mul` mapVar f b
mapVar f (a `Raise` b)      = mapVar f a `Raise` mapVar f b
mapVar f (a `Divide` b)     = mapVar f a `Divide` mapVar f b
mapVar f (Application fn e) = Application fn (mapVar f e)
mapVar _ e                  = e

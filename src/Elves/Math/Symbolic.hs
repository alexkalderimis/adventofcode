module Elves.Math.Symbolic where

import Data.Text (Text)
import Data.HashSet (HashSet)
import qualified Data.HashSet as S

import qualified Debug.Trace as Debug

rule n = id -- Debug.trace ("rule " ++ n)

type Set = HashSet

infixl 4 :+:
infixl 5 :*:, :/:
infixr 6 :^:

type Variable = Text

data Expr a = Var Variable
             | NaN
             | Constant a 
             | (Expr a) :+: (Expr a) 
             | (Expr a) :*: (Expr a)
             | (Expr a) :^: (Expr a)
             | (Expr a) :/: (Expr a)
             | Application Fun (Expr a)
             deriving (Show, Eq)

instance (Num a) => Num (Expr a) where
  (+) = (:+:)
  (*) = (:*:)
  a - b = a :+: negate' b
  negate = negate'
  fromInteger = Constant . fromInteger
  abs = Application Absolute
  signum = Application SigNum

instance (Fractional a) => Fractional (Expr a) where
  (/) = (:/:)
  fromRational = Constant . fromRational

instance (Floating a) => Floating (Expr a) where
  pi = Constant pi
  exp = Application Exp
  log = Application Logn
  sqrt = squareRootOf
  (**) = (:^:)
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
  deriving (Show, Eq)

variables :: Expr a -> Set Variable
variables (Var v) = S.singleton v
variables (a :+: b) = variables a <> variables b
variables (a :*: b) = variables a <> variables b
variables (a :/: b) = variables a <> variables b
variables (a :^: b) = variables a <> variables b
variables _ = S.empty

bottomUp :: (Expr a -> Expr a) -> Expr a -> Expr a
bottomUp f e = f (applyChildren f e)
  where
    applyChildren f e = case e of
              a :+: b -> bottomUp f a :+: bottomUp f b
              a :*: b -> bottomUp f a :*: bottomUp f b
              a :^: b -> bottomUp f a :^: bottomUp f b
              a :/: b -> bottomUp f a :/: bottomUp f b
              Application g a -> Application g (bottomUp f a)
              _ -> e

simplify :: (Num a, Eq a, Floating a) => Expr a -> Expr a
simplify = bottomUp go
  where go e = case e of
          -- NaN propagation:
          (NaN :+: _  ) -> NaN
          (_   :+: NaN) -> NaN
          (NaN :*: _  ) -> NaN
          (_   :*: NaN) -> NaN
          (NaN :/: _  ) -> NaN
          (_   :/: NaN) -> NaN
          (NaN :^: _  ) -> NaN
          (_   :^: NaN) -> NaN
          Application _ NaN -> NaN

          Application f (Constant x) -> Constant $ case f of
                                          Absolute -> abs x
                                          SigNum -> signum x
                                          Sin -> sin x
                                          Cos -> cos x
                                          Tan -> tan x
                                          ASin -> asin x
                                          ACos -> acos x
                                          ATan -> atan x
                                          SinH -> sinh x
                                          CosH -> cosh x
                                          TanH -> tanh x
                                          ASinH -> asinh x
                                          ACosH -> acosh x
                                          ATanH -> atanh x
                                          Exp -> exp x
                                          Logn -> log x

          ((x :+: Constant a) :+: (y :+: Constant b)) -> rule "+1" $ x + y + Constant (a + b)
          (a :+: b) | a == b -> rule "+2" $ 2 * b
          (Constant 0 :+: a         ) -> rule "+zero-elimination-lhs" a
          (a          :+: Constant 0) -> rule "+zero-elimination-rhs" a
          (Constant a :+: Constant b) -> rule "+constants" Constant (a + b)
          ((expr :+: Constant a) :+: Constant b) -> rule "+constant folding" $ expr :+: Constant (a + b)
          (((Constant x :*: a) :+: (Constant y :*: b))
             :+: ((Constant x' :*: a') :+: (Constant y' :*: b')))
             | (a,b) == (a',b') -> rule "+multiplication of same"
                                    $ (Constant (x + x') :*: a)
                                       :+:
                                       (Constant (y + y') :*: b)
          (Constant a :+: expr) -> rule "+reorder" (expr :+: Constant a)

          ((Constant x :*: a) :*: (Constant y :*: b)) -> rule "*constant folding" $ Constant (x * y) :*: (a :*: b)
          (Constant a :*: (Constant b :*: expr))      -> rule "*constant folding-2" $ (Constant $ a*b) :*: expr
          (Constant a :*: Constant b)                 -> rule "*constant folding-3" $ Constant (a*b)
          (Constant 1 :*: a)       -> rule "* 1 elimination" a
          (Constant 0 :*: a)       -> rule "* 0 elimination" 0
          (a :*: Constant x)       -> rule "* reorder" $ Constant x :*: a -- swap to make equalities work
          (Constant a :*: (Var x :*: e)) -> rule "* var lifting" $ (Constant a :*: Var x) * e
          (Constant _ :*: _)       -> e
          (Var a :*: Var b) | b < a -> rule "* var reorder" $ Var b :*: Var a
          (expr :*: Var x)  -> rule "* var precedence" (Var x * expr)

          (a :*: b) | a == b -> a :^: Constant 2

          (Constant a :^: Constant b) -> Constant (a ** b)
          (a          :^: Constant 1) -> a
          (a          :^: Constant 0) -> 1

          ((a :^: Constant y) :*: a')
            | a == a' -> a :^: Constant (y + 1)
          ((a :^: Constant y) :*: (a' :^: Constant y'))
            | a == a' -> a :^: Constant (y + y')

          ((x :^: Constant a) :^: Constant b)  -> x :^: Constant (a*b)

          (Constant 0 :/: a      )    -> Constant 0
          (Constant a :/: Constant 0) -> NaN
          (Constant a :/: Constant b) -> Constant (a / b)
          (a          :/: Constant 1) -> a
          
          _ -> e

fullSimplify :: (Num a, Eq a, Floating a) => Expr a -> Expr a
fullSimplify = firstStable . iterate simplify 
  where
    firstStable xs = fst . head . dropWhile (uncurry (/=)) $ zip xs (tail xs)

negate' :: (Num a) => Expr a -> Expr a
negate' (Var c)    = Constant (-1) :*: Var c
negate' (Constant a)  = Constant (negate a)
negate' (a :+: b)  = negate' a :+: negate' b
negate' (a :*: b)  = negate' a :*: b
negate' (a :^: b)  = Constant (-1) :*: a :^: b
negate' (a :/: b)  = negate' a :/: b

mapVar :: (Variable -> Expr a) -> Expr a -> Expr a
mapVar f (Var x)   = f x
mapVar _ (Constant a) = Constant a
mapVar f (a :+: b) = mapVar f a :+: mapVar f b
mapVar f (a :*: b) = mapVar f a :*: mapVar f b
mapVar f (a :^: b) = mapVar f a :^: mapVar f b
mapVar f (a :/: b) = mapVar f a :/: mapVar f b

plugIn :: Variable -> a -> Expr a -> Expr a
plugIn c val = mapVar (\x -> if x == c then Constant val else Var x)

evalExpr :: (Num a, Floating a, Eq a) => Variable -> a -> Expr a -> Expr a
evalExpr var value  = fullSimplify. plugIn var value

derivative :: (Num a, Floating a) => Expr a -> Expr a
derivative (Var c)         = Constant 1
derivative (Constant x)    = Constant 0
--product rule (ab' + a'b)
derivative (a :*: b)       = (a :*: derivative b) :+: (b :*: derivative a) -- product rule
 --power rule (xa^(x-1) * a')
derivative (a :^: b)       = (b * (a :^: (b - 1))) * derivative a
-- sum of derivatives
derivative (a :+: b)       = derivative a :+: derivative b
 -- quotient rule ( (a'b - b'a) / b^2 )
derivative (a :/: b)       = ((derivative a :*: b) :+: negate' (derivative b :*: a))
                               :/: squareOf b
derivative (Application f a) = dfun * derivative a
  where
    dfun = case f of Absolute -> a / abs a
                     SigNum -> 0 * (1 / signum a)
                     Sin -> cos a
                     Cos -> 1 - sin a
                     Tan -> 1 / squareOf (cos a)
                     ASin -> 1 / sqrt (1 - squareOf a)
                     ACos -> negate (1 / sqrt (1 - squareOf a))
                     ATan -> 1 / (squareOf a + 1)
                     SinH -> cosh a
                     CosH -> sinh a
                     TanH -> 1 - squareOf (tanh a)
                     ASinH -> 1 / sqrt (squareOf a + 1)
                     ACosH -> 1 / sqrt (squareOf a - 1)
                     ATanH -> 1 / (1 - squareOf a)
                     Exp -> exp a -- derivative of exponentiol is the exponentiol
                     Logn -> 1 / a

squareOf :: (Num a) => Expr a -> Expr a
squareOf x = x `raisedTo` 2

raisedTo :: (Num a) => Expr a -> a -> Expr a
raisedTo x y = x :^: Constant y

squareRootOf :: (Floating a) => Expr a -> Expr a
squareRootOf x = x :^: Constant 0.5

minus :: (Num a) => Expr a -> a -> Expr a
minus expr x = expr :+: Constant (negate x)

dx :: (Num a, Floating a, Eq a) => Expr a -> Expr a
dx = fullSimplify . derivative

eval :: (Num a, Eq a, Floating a) => [(Variable, a)] -> Expr a -> Either String a
eval bindings expr
  | not (S.null unbound) = Left $ "unbound variables: " <> show unbound
  | otherwise  = case fullSimplify expr' of
                   Constant x -> Right x
                   NaN -> Left "NaN"
                   _ -> Left "impossible"
  where
    expr' = bindall bindings expr
    unbound = variables expr'

bindall :: Foldable t => t (Variable, a) -> Expr a -> Expr a
bindall bs e = foldr (uncurry plugIn) e bs

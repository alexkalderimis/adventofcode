module Elves.Math.Simplify where

import Elves.Math.Expression

-- import qualified Debug.Trace as Debug
rule n = id -- Debug.trace ("rule " ++ n)
-- rule n = Debug.trace ("rule " ++ n)

fullSimplify :: (Num a, Eq a, Floating a, RealFloat a) => Expr a -> Expr a
fullSimplify = firstStable . iterate simplify 
  where
    firstStable xs = fst . head . dropWhile (uncurry (/=)) $ zip xs (tail xs)

simplify :: (Num a, Eq a, RealFloat a, Floating a) => Expr a -> Expr a
simplify = bottomUp go
  where go e = case e of
          -- NaN propagation:
          (NaN `Add` _  ) -> NaN
          (_   `Add` NaN) -> NaN
          (NaN `Mul` _  ) -> NaN
          (_   `Mul` NaN) -> NaN
          (NaN `Divide` _  ) -> NaN
          (_   `Divide` NaN) -> NaN
          (NaN `Raise` _  ) -> NaN
          (_   `Raise` NaN) -> NaN
          Application _ NaN -> NaN
          (Constant x) | isNaN x -> NaN

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

          (Constant a `Add` (Constant b `Add` Var x)) -> rule "+constant folding" $ Constant (a + b) * Var x
          ((x `Add` Constant a) `Add` (y `Add` Constant b)) -> rule "+1" $ x + y + Constant (a + b)
          (a `Add` b) | a == b -> rule "+2" $ 2 * b
          (Constant 0 `Add` a         ) -> rule "+zero-elimination-lhs" a
          (a          `Add` Constant 0) -> rule "+zero-elimination-rhs" a
          (Constant a `Add` Constant b) -> rule "+constants" Constant (a + b)
          ((expr `Add` Constant a) `Add` Constant b) -> rule "+constant folding" $ expr `Add` Constant (a + b)
          (((Constant x `Mul` a) `Add` (Constant y `Mul` b)) `Add` ((Constant x' `Mul` a') `Add` (Constant y' `Mul` b')))
             | (a,b) == (a',b') -> rule "+multiplication of same"
                                    $ (Constant (x + x') `Mul` a) `Add` (Constant (y + y') `Mul` b)
          (Constant a `Add` expr) -> rule "+reorder" (expr `Add` Constant a)

            
          (Constant a `Mul` Constant b) -> rule "constant * constant" $ Constant (a*b)
          (Constant a `Mul` (x `Add` Constant b)) -> rule "* distributive" $ Constant (a * b) + (Constant a * x)
          (Var x `Mul` (Constant a `Mul` Var x')) | x == x' -> Constant a * (Var x `Raise` Constant 2)
          (Add a b `Mul` Add c d) -> rule "FOIL" $ sum [p * q | p <- [a, b], q <- [c, d]]
          ((Constant a `Mul` x) `Add` (Constant b `Mul` x')) | x == x' -> rule "x.a + y.a -> (x+y).a" $ Constant (a + b) * x
          ((Constant x `Mul` a) `Mul` (Constant y `Mul` b)) -> rule "*constant folding" $ Constant (x * y) `Mul` (a `Mul` b)
          (Constant a `Mul` (Constant b `Mul` expr))        -> rule "*constant folding-2" $ (Constant $ a*b) `Mul` expr
          (Constant 1 `Mul` a)       -> rule "* 1 elimination" a
          (Constant 0 `Mul` a)       -> rule "* 0 elimination" 0
          (a `Mul` Constant x)       -> rule "* reorder" $ Constant x `Mul` a -- swap to make equalities work
          -- (Constant a `Mul` (Var x `Mul` e)) -> rule "* var lifting" $ (Constant a `Mul` Var x) * e
          (Constant _ `Mul` _)       -> e
          (Var a `Mul` Var b) | b < a -> rule "* var reorder" $ Var b `Mul` Var a
                              | a == b -> rule "a * a -> a^2" $ Var a `Raise` Constant 2
                              | otherwise -> e
          (expr `Mul` Var x)  -> rule "* var precedence" (Var x * expr)
          (Mul a (Mul a' x)) | a == a' -> rule "a * a * x -> a^2 * x" $ (a ** 2) * x

          (a `Mul` b) | a == b -> a ** 2

          (a          `Raise` Constant 1) -> a
          (a          `Raise` Constant 0) -> 1
          (Constant a `Raise` Constant b) -> Constant (a ** b)

          ((a `Raise` Constant y) `Mul` a')
            | a == a' -> a `Raise` Constant (y + 1)
          ((a `Raise` Constant y) `Mul` (a' `Raise` Constant y'))
            | a == a' -> a `Raise` Constant (y + y')

          ((x `Raise` Constant a) `Raise` Constant b)  -> x `Raise` Constant (a*b)

          (a `Divide` (b `Divide` c)) -> rule "Division tower-1: a/(b/c) -> ac/b" $ (a * c) / b
          ((a `Divide` b) `Divide` c) -> rule "Division tower-2: (a/b)/c -> a/bc" $ a / (b * c)
          (Mul a b    `Divide` Mul a' b' ) | a == a' -> rule "Divide: a.b/a.c -> b/c" $ b / b'
                                           | b == b' -> rule "Divide: a.b/c.b -> a/c" $ a / a'
          (x          `Divide` Mul a b   ) | x == a -> rule "Divide: a/(a.b) -> 1/b" $ 1 / b
                                           | x == b -> rule "Divide: a/(b.a) -> 1/a" $ 1 / a
          (Mul a b    `Divide` x         ) | x == a -> rule "Divide: a.b/a -> b" b
                                           | x == b -> rule "Divide: a.b/b -> a" a
                                           | otherwise -> rule "Divide: Float out - a.b/c -> a . b/c" $ a * (b / x)
          (a          `Divide` b         ) | a == b -> rule "Divide: a/a -> 1" $ Constant 1
          (Constant 0 `Divide` a         )    -> Constant 0
          (Constant a `Divide` Constant 0) -> NaN
          (Constant a `Divide` Constant b) -> Constant (a / b)
          (a          `Divide` Constant 1) -> a

          (Raise a (Constant b) `Add` Raise c (Constant d)) | d < b -> rule "order of exponents" $ (c ** Constant d) + (a ** Constant b)
          (a `Add` Raise c (Constant d)) -> rule "exponents before none" $ (c ** Constant d) + a
          (lhs@(Mul _ (Raise _ (Constant b))) `Add` rhs@(Mul _ (Raise _ (Constant d))))
            | d < b -> rule "order of exponents, nested" $ rhs + lhs
           
          (a `Add` Mul x (Raise c (Constant d))) -> rule "exponents before none" $ (x * (c ** Constant d)) + a
          
          _ -> e


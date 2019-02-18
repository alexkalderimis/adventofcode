module Elves.Math.Differentiation where

import Elves.Math.Expression

dx :: (Num a, RealFloat a, Floating a, Eq a) => Variable -> Expr a -> Expr a
dx v = derivative . mapVar fix
  where
    fix x = pure . (if v == x then Var else Fixed) $ x

derivative :: (Num a, Floating a) => Expr a -> Expr a
derivative (Var c)         = Constant 1
derivative (Constant x)    = Constant 0
derivative (Fixed    x)    = Constant 0
--product rule (ab' + a'b)
derivative (a `Mul` b)       = (a `Mul` derivative b) `Add` (b `Mul` derivative a) -- product rule
 --power rule (xa^(x-1) * a')
derivative (a `Raise` b)       = (b * (a `Raise` (b - 1))) * derivative a
-- sum of derivatives
derivative (a `Add` b)       = derivative a `Add` derivative b
 -- quotient rule ( (a'b - b'a) / b^2 )
derivative (a `Divide` b) = ((derivative a `Mul` b) `Add` negate' (derivative b `Mul` a))
                               `Divide` squareOf b
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


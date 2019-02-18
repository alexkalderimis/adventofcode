module Elves.Math.Pretty where

import           Data.Text             (unpack)

import           Elves.Math.Expression

-- show expressions in more readable form
pretty :: (Show a, Num a, Ord a, Fractional a) => Expr a -> String
pretty (Application Sin x `Raise` Constant 2) = "sin" <> superscript 2 <> "(" <> pretty x <> ")"
pretty (Application Sin x) = "sin(" <> pretty x <> ")"
pretty (Application Cos x) = "cos(" <> pretty x <> ")"
pretty (Application Tan x) = "tan(" <> pretty x <> ")"
pretty (Application Exp (Constant x)) = "e" <> superscript x
pretty ((a `Add` b) `Mul` rhs) = "(" <> pretty (a + b) <> ") \x22C5 (" <> pretty rhs <> ")"
pretty (Constant a `Mul` Var x) = showval a <> unpack x
pretty (Var x `Raise` Constant 0.5) = "\x221A(" <> unpack x <> ")"
pretty (Var x `Raise` Constant e) = unpack x <> superscript e
pretty (x `Raise` Var e) = let lhs = case x of
                                 Var v      -> unpack v
                                 Constant c -> showval c
                                 _          -> "(" <> pretty x <> ")"
                       in lhs <> superscripted (unpack e)
pretty (Constant a `Mul` (Var x `Raise` Constant 0.5)) = "\x221A(" <> showval a <> unpack x <> ")"
pretty (x `Raise` Constant e) = "(" <> pretty x <> ")" <> superscript e
pretty (x `Raise` e) = let lhs = case x of
                               Var v      -> unpack v
                               Constant c -> showval c
                               _          -> "(" <> pretty x <> ")"
                    in lhs <> "^(" <> pretty e <> ")"
pretty (Constant a `Mul` (Var x `Raise` Constant e)) = showval a <> unpack x <> superscript e
pretty (lhs `Add` (Constant a `Mul` rhs)) | a < 0 = pretty lhs <> " - " <> pretty (Constant (abs a) * rhs)
pretty (lhs `Add` Constant a) | a < 0 = pretty lhs <> " - " <> showval (abs a)
pretty (lhs `Add` rhs) = pretty lhs <> " + " <> pretty rhs
pretty (lhs `Mul` rhs) = pretty lhs <> " \x22C5 " <> pretty rhs
pretty (lhs `Divide` rhs) = pretty lhs <> " / " <> pretty rhs
pretty (Constant x) = showval x
pretty (Var x) = unpack x
pretty (Fixed x) = unpack x

showval :: Show a => a -> String
showval = strpEnd . show
  where
    strpEnd = reverse . dropDot . dropWhile (== '0') . reverse
    dropDot ('.':s) = s
    dropDot s       = s

superscript :: Show a => a -> String
superscript = superscripted . showval

superscripted :: String -> String
superscripted = fmap replace
  where
    replace '.' = '\x22C5'
    replace '1' = '\x00B9'
    replace '2' = '\x00B2'
    replace '3' = '\x00B3'
    replace '4' = '\x2074'
    replace '5' = '\x2075'
    replace '6' = '\x2076'
    replace '7' = '\x2077'
    replace '8' = '\x2078'
    replace '9' = '\x2079'
    replace '0' = '\x2070'
    replace '-' = '\x207B'
    replace '+' = '\x207A'
    replace 'a' = 'ᵃ'
    replace 'b' = 'ᵇ'
    replace 'c' = 'ᶜ'
    replace 'd' = 'ᵈ'
    replace 'e' = 'ᵉ'
    replace 'f' = 'ᶠ'
    replace 'g' = 'ᵍ'
    replace 'h' = 'ʰ'
    replace 'i' = 'ⁱ'
    replace 'j' = 'ʲ'
    replace 'k' = 'ᵏ'
    replace 'l' = 'ˡ'
    replace 'm' = 'ᵐ'
    replace 'n' = 'ⁿ'
    replace 'o' = 'ᵒ'
    replace 'p' = 'ᵖ'
    replace 'r' = 'ʳ'
    replace 's' = 'ˢ'
    replace 't' = 'ᵗ'
    replace 'u' = 'ᵘ'
    replace 'v' = 'ᵛ'
    replace 'w' = 'ʷ'
    replace 'x' = 'ˣ'
    replace 'y' = 'ʸ'
    replace 'z' = 'ᶻ'
    replace c   = c


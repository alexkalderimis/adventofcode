{-# LANGUAGE OverloadedStrings #-}

module Elves.Math.Symbolic where

import Data.Containers.ListUtils (nubOrd)
import qualified Data.List as L
import Control.Applicative
import Data.Maybe
import Data.Monoid
import Data.Text (Text, unpack, pack)
import qualified Data.HashSet as S
import qualified Data.HashMap.Strict as M

import Elves.Math.Expression
import Elves.Math.Simplify
import Elves.Math.Differentiation
import Elves.Math.Pretty

plugIn :: Variable -> a -> Expr a -> Expr a
plugIn c val = mapVar $ \x -> if x == c
                                 then Just (Constant val)
                                 else Nothing

evalExpr :: (Num a, RealFloat a, Floating a, Eq a) => Variable -> a -> Expr a -> Expr a
evalExpr var value  = fullSimplify. plugIn var value

eval :: (Num a, Eq a, RealFloat a, Floating a) => [(Variable, a)] -> Expr a -> Either String a
eval bindings expr
  | not (S.null unbound) = Left $ "unbound variables: " <> show unbound
  | otherwise  = case fullSimplify expr' of
                   Constant x -> Right x
                   NaN        -> Left "NaN"
                   _          -> Left "impossible"
  where
    expr' = bindall bindings expr
    unbound = variables expr'

bindall :: Foldable t => t (Variable, a) -> Expr a -> Expr a
bindall bs e = foldr (uncurry plugIn) e bs


wobble =
  let x = var "x"
      y = var "y"
      lam = var "l"
   in [1 + (2 * lam * x), 1 + (2 * lam * y), (x ** 2) + (y ** 2) - 1]

{-
wibble :: (Eq a, Floating a, Ord a) => [Expr a] -> M.HashMap Variable [Expr a]
wibble es =
  let subs = fmap fullSimplify <$> unify es
      goodSubs = M.fromListWith (<>) (fmap pure <$> filter (\(x,e) -> not . S.member x $ variables e) subs)
      applyGoodSubs (v, e) = case M.lookup v goodSubs of
                               Nothing -> (v, [e])
                               Just es -> (v, nubOrd $ fmap (\s -> mapVar (\v' -> if v == v' then Just s else Nothing) e) es)

   in M.fromListWith (<>) (fmap applyGoodSubs subs)

nSolved :: M.HashMap Variable [Expr a] -> Int
nSolved = getSum . foldMap (\es ->
  if any ((== 0) . S.size . variables) es
     then Sum 1
     else Sum 0)

rabble :: (Eq a, Floating a, Ord a)
       => M.HashMap Variable [Expr a] -> M.HashMap Variable [Expr a]
rabble m = M.fromListWith (<>) (M.toList m >>= f)
  where
    f (k, es) = (k,es) : M.toList (wibble es)
-}

unify :: (Eq a, RealFloat a, Floating a) => [Expr a] -> [(Variable, Expr a)]
unify [] = []
unify es = concat $ L.unfoldr solve [ eq | (a:bs) <- L.tails (fmap fullSimplify es), b <- bs
                                    , eq <- [(a,b),(b,a)]
                                    ]
  where
    solve eqs = do (eq, eqs') <- L.uncons eqs
                   return (sub eq, eqs')

sub :: (Eq a, RealFloat a, Floating a) => (Expr a, Expr a) -> [(Variable, Expr a)]
sub = flip appEndo [] . go
  where
    go (lhs, e) = case lhs of
      -- this is a solution
      Var a -> Endo ((a, e) :)

      -- unpack the operation, and keep going
      (a `Add` b)    -> go (a, e - b) <> go (b, e - a)
      (a `Mul` b)    -> go (a, e / b) <> go (b, e / a)
      (a `Divide` b) -> go (a, e * b) <> go (e * b, a)
      (a `Raise` b)  -> go (a, e `Raise` (1 / b))
                        <>
                        go (log (a `Raise` b), log e)
      -- function solving rules
      (Application Logn (a `Raise` b)) -> go (b * log a, e)
      (Application Logn x) -> go (x, exp 1 ** e)
      (Application Exp x) -> go (Constant (exp 1) ** x, e)
      (Application Sin x) -> go (x, asin e + 2 * pi)
                                 <>
                                 go (x, pi + asin (negate e) + 2 * pi)
      (Application Cos x) -> go (x, acos e + 2 * pi)
                                 <>
                                 go (x, negate (acos e) + 2 * pi)
      (Application Tan x) -> go (x, atan e + pi)
      -- signum and abs do not provide enough information about their argument to
      -- allow us to solve - i.e they are not injective

      -- can we go further by simplification? if so then keep going
      _ -> let lhs' =  simplify lhs
            in if lhs' == lhs then mempty else go (lhs', e)


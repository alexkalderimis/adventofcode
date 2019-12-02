module Elves.Math.Solution where

import Data.Semigroup

data Bound a = Including a | Excluding a
  deriving (Show, Eq)

data Solution a
  = Exactly a
  | LessThan a
  | GreaterThan a
  | OneOf [Solution a]
  | Not (Solution a)
  | And (Solution a) (Solution a)
  deriving (Show, Eq)

instance Semigroup (Solution a) where

  (OneOf []) <> a = a
  a <> (OneOf []) = a

  (OneOf xs) <> (OneOf ys) = OneOf (xs <> ys)

  a <> OneOf xs = OneOf (a : xs)
  OneOf xs <> a = OneOf (a : xs)

  (Not a) <> (Not b) = Not (a `And` b)

  a <> b = OneOf [a,b]

lteq :: a -> Solution a
lteq a = OneOf [Exactly a, LessThan a]

gteq :: a -> Solution a
gteq a = OneOf [Exactly a, GreaterThan a]

inRange :: Bound a -> Bound a -> Solution a
inRange (Including a) (Including b) = OneOf [ Exactly a, Exactly b 
                                            , And (GreaterThan a)
                                                  (LessThan b)
                                            ]
inRange (Excluding a) (Including b) = OneOf [ Exactly b
                                            , And (GreaterThan a)
                                                  (LessThan b)
                                            ]
inRange (Including a) (Excluding b) = OneOf [ Exactly a 
                                            , And (GreaterThan a)
                                                  (LessThan b)
                                            ]
inRange (Excluding a) (Excluding b) = GreaterThan a `And` LessThan b

isnt :: a -> Solution a
isnt = Not . OneOf . pure . Exactly


module Elves.Cartesian where

-- a point on a 2D integral cartestian place as (Y,X)
type Location = (Int, Int)
data Direction = North | East | South | West deriving (Show, Eq, Bounded, Enum)

translate :: Location -> Location -> Location
translate (dy,dx) (y,x) = (y + dy, x + dx)

adjacent4 :: Location -> [Location]
adjacent4 pos = fmap ($ pos) [up,down,left,right]

adjacent8 :: Location -> [Location]
adjacent8 pos = fmap ($ pos) [up,down,left,right
                             ,up . right, up . left
                             ,down . right, down . left
                             ]

up, down, left, right :: Location -> Location
up    = move North 1
down  = move South 1
right = move East 1
left  = move West 1

x :: Location -> Int
x = snd

y :: Location -> Int
y = fst

move :: Direction -> Int -> Location -> Location
move North n = translate (negate n, 0)
move South n = translate (       n, 0)
move West  n = translate (0, negate n)
move East  n = translate (0,        n)

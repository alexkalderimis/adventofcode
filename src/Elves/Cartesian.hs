module Elves.Cartesian where

type Location = (Int, Int)

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
up    = translate (-1, 0)
down  = translate ( 1, 0)
left  = translate (0,  1)
right = translate (0, -1)

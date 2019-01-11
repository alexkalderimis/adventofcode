module Elves.Cartesian where

type Location = (Int, Int)

translate :: Location -> Location -> Location
translate (dy,dx) (y,x) = (y + dy, x + dx)

up, down, left, right :: Location -> Location
up    = translate (-1, 0)
down  = translate ( 1, 0)
left  = translate (0,  1)
right = translate (0, -1)

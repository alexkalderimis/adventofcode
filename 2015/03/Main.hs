{-# LANGUAGE OverloadedStrings #-}

import qualified Data.List       as L

import           Elves
import           Elves.Advent
import           Elves.Cartesian
import           Elves.CountMap

type Instruction = (Location -> Location)
type Houses = CountMap Location

main :: IO ()
main = day 3 (some moveP) (print.housesVisited) (print.usingRoboSanta) test

test = do
  describe "pt1" $ do
    let egs = [(">", 2)
              ,("^>v<", 4)
              ,("^v^v^v^v^v", 2)
              ]
    testing "houses-visited" (housesVisited <$> some moveP) (namedExamples egs)
  describe "pt2" $ do
    let egs = [(">v", 3)
              ,("^>v<", 3)
              ,("^v^v^v^v^v", 11)
              ]
    testing "houses-visited" (usingRoboSanta <$> some moveP) (namedExamples egs)

housesVisited :: [Instruction] -> Int
housesVisited = size . visitAll

usingRoboSanta :: [Instruction] -> Int
usingRoboSanta ms = let (xs,ys) = unterleave ms in size (visitAll xs <> visitAll ys)

visitAll :: [Instruction] -> Houses
visitAll = fst . L.foldl' visit (next origin mempty)
  where
    visit (m,pos) f = next (f pos) m
    next pos m      = (count pos m, pos)
    origin          = (0,0)

moveP =  (up    <$ "^")
     <|> (down  <$ "v")
     <|> (left  <$ "<")
     <|> (right <$ ">")

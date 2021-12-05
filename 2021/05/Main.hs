{-# LANGUAGE OverloadedStrings, GeneralizedNewtypeDeriving #-}

import qualified Data.Text as T
import           Data.Text (Text)
import           Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Attoparsec.Text as A
import Text.Parser.Combinators (sepBy1)
import Text.Parser.Char (newline, space, text)

import Elves
import Elves.Advent
import qualified Elves.CountMap as CountMap

data Point = Point { x :: !Int, y :: !Int }
  deriving (Show, Eq, Ord)

data Line = Line { from :: !Point, to :: !Point }
  deriving (Eq)

instance Show Line where
  show line = mconcat [ show . x $ from line
                      , ","
                      , show . y $ from line
                      , " -> "
                      , show . x $ to line
                      , ","
                      , show . y $ to line
                      ]

main :: IO ()
main = day 05 parser pt1 pt2 test
  where
    parser = lineP `sepBy1` newline
    pt1 = print . length . intersections . filter (\l -> horizontal l || vertical l)
    pt2 = print . length . intersections

width :: Line -> Int
width line = abs $ x (to line) - x (from line)

height :: Line -> Int
height line = abs $ y (to line) - y (from line)

horizontal :: Line -> Bool
horizontal line = height line == 0

vertical :: Line -> Bool
vertical line = width line == 0

points :: Line -> [Point]
points line
  | horizontal line = let y' = y (from line)
                          start = x (from line) `min` x (to line)
                          end = x (from line) `max` x (to line)
                       in [Point x y' | x <- [start .. end]]
  | vertical line = let x' = x (from line)
                        start = y (from line) `min` y (to line)
                        end = y (from line) `max` y (to line)
                     in [Point x' y | y <- [start .. end]]
  | width line == height line = -- i.e., rising or falling 1 in 1
                 let (start, end) = if x (from line) <= x (to line)
                                      then (from line, to line)
                                      else (to line, from line)
                     gradient = if y end > y start then 1 else -1
                 in [Point (x start + d) (y start + gradient * d) | d <- [0 .. width line]]
  | otherwise = error ("Not vertical, horizontal or diagonal: " <> show line)

intersections :: [Line] -> [Point]
intersections = CountMap.counting (> 1) . CountMap.fromList . (>>= points)

lineP :: Parser Line
lineP = Line <$> (point <* text " -> ") <*> point
  where
    point = Point <$> (A.decimal <* text ",") <*> A.decimal

test = do
  let testInput = [ "0,9 -> 5,9"
              , "8,0 -> 0,8"
              , "9,4 -> 3,4"
              , "2,2 -> 2,1"
              , "7,0 -> 7,4"
              , "6,4 -> 2,0"
              , "0,9 -> 2,9"
              , "3,4 -> 1,4"
              , "0,0 -> 8,8"
              , "5,5 -> 8,2"
              ]
  let ePoints = mapM (parseOnly lineP) testInput

  it "finds the correct intersections, pt1" $ do
    let expected = [Point 3 4, Point 7 4, Point 0 9, Point 1 9, Point 2 9]

    fmap (Set.fromList . intersections . filter (\l -> horizontal l || vertical l)) ePoints 
      `shouldBe` Right (Set.fromList expected)

  it "finds the correct number of intersections, pt2" $ do
    fmap (length . intersections) ePoints `shouldBe` Right 12

  it "can generate sets of points for vertical lines" $ do
    points (Line (Point 1 1) (Point 1 3)) `shouldBe` [Point 1 1, Point 1 2, Point 1 3]
  it "can generate sets of points for horizontal lines" $ do
    points (Line (Point 9 7) (Point 7 7)) `shouldBe` [Point 7 7, Point 8 7, Point 9 7]
  it "can generate sets of points for diagonal lines" $ do
    points (Line (Point 1 1) (Point 3 3)) `shouldBe` [Point 1 1, Point 2 2, Point 3 3]
    points (Line (Point 9 7) (Point 7 9)) `shouldBe` [Point 7 9, Point 8 8, Point 9 7]

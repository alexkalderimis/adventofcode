{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}

import qualified Data.List as L
import qualified Data.Text as T
import           Data.Text (Text)
import qualified Data.Attoparsec.Text as A
import qualified Data.Array as Array
import           Data.Array (Array, Ix)
import           Control.Applicative.Combinators
import           Data.Attoparsec.Text            (digit)
import qualified Data.Map.Strict as M
import           Data.Map.Strict (Map)
import qualified Data.Set as Set
import           Data.Set (Set, (\\))

import Elves
import Elves.Advent
import Elves.StrictGrid

type Coord = Coordinate
newtype Depth = Depth { getDepth :: Int } deriving (Show, Eq, Ord, Enum)
type Basin = Set Coordinate

newtype HeightMap = HM { heightMap :: Array Coordinate Depth } deriving (Show, Eq)

main :: IO ()
main = day 09 parser pt1 pt2 test
  where
    pt1 = print . risk
    pt2 = print . solution . basins

test = do
  let Right hm = parseOnly parser exampleInput
  it "finds the right low points" $ do
    localMinima hm `shouldBe` [(Coord (Row 0) (Col 1), Depth 1)
                              ,(Coord (Row 0) (Col 9), Depth 0)
                              ,(Coord (Row 2) (Col 2), Depth 5)
                              ,(Coord (Row 4) (Col 6), Depth 5)
                              ]
  it "calculates the correct risk" $ do
    risk hm `shouldBe` 15

  describe "basins" $ do
    let bs = basins hm

    it "finds four basins" $ do
      length bs `shouldBe` 4

    it "finds the right members" $ do
      bs `shouldContain` [ Set.fromList [Coord (Row 0) (Col 0), Coord (Row 0) (Col 1), Coord (Row 1) (Col 0)] ]
      bs `shouldContain` [ Set.fromList $ mconcat [ [Coord (Row 0) (Col c) | c <- [5..9]]
                                                  , [Coord (Row 1) (Col c) | c <- [6, 8, 9]]
                                                  , [Coord (Row 2) (Col 9) ]
                                                  ]
                         ] 
      bs `shouldContain` [ Set.fromList $ mconcat [ [Coord (Row 4) (Col c) | c <- [5..9]]
                                                  , [Coord (Row 3) (Col c) | c <- [6..8]]
                                                  , [Coord (Row 2) (Col 7) ]
                                                  ]
                         ] 

    it "has the right solution" $ do
      solution bs `shouldBe` 1134

parser :: Parser HeightMap
parser = HM <$> gridP (Depth . read . pure <$> digit)

exampleInput :: Text
exampleInput =
  --            0123456789
  let lines = ["2199943210" -- 0
              ,"3987894921" -- 1
              ,"9856789892" -- 2
              ,"8767896789" -- 3
              ,"9899965678" -- 4
              ]
   in T.intercalate "\n" lines

localMinima :: HeightMap -> [(Coordinate, Depth)]
localMinima hm = do
  let bs = Array.bounds (heightMap hm)
  (loc, d) <- Array.assocs $ heightMap hm
  let locs = neighbours hm loc
      vals = (heightMap hm Array.!) <$> locs
  guard (all (> d) vals)
  pure (loc, d)

neighbours :: HeightMap -> Coord -> [Coord]
neighbours hm = nextCoords False (Array.bounds $ heightMap hm)

risk :: HeightMap -> Int
risk = sum . fmap (getDepth . succ . snd) . localMinima

basins :: HeightMap -> [Basin]
basins hm = fmap (basin hm) $ localMinima hm

-- the problem guarantees that basins do not overlap
basin :: HeightMap -> (Coord, Depth) -> Basin
basin hm (loc, d) = let b = Set.singleton loc in go b b
  where
    go frontier !b = let frontier' = expand frontier \\ b
                      in if Set.null frontier
                         then b
                         else go frontier' (b <> frontier')

    expand = foldMap $ \loc -> let d = heightMap hm Array.! loc
                                in Set.fromList . filter (above d) $ neighbours hm loc

    above d loc = let h = heightMap hm Array.! loc
                   in d <= h && h < Depth 9

solution :: [Basin] -> Int
solution = product . take 3 . L.sortOn Down . fmap Set.size

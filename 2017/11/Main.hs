{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

-- hex-grid co-ordinates:
--
--    \    /     \      /     \      /
--     +--+       +----+       +----+  --+
--    /    \____ / -1,0 \____ /      \   |  row -1
--    \    /-1,-1\  n   /-1,1 \      /   |
--     +--+ nw    +----+  ne   +----+  --+
--    /    \     /      \     /      \   |
--  -+ 0,-2 +---+  0,0   +---+ 0,2    +  | row  0
--    \    /0,-1 \      /0,1  \      /   |
--     +--+ sw    +----+  se   +----+  --+
--    /    \     /  s   \     /      \
--  -+ 1,-2 +---+  1,0   +---+ 1,2    +  | row  0
--    \    /1,-1 \      /1,1  \      /   |
--     +--+       +----+       +----+  --+
--    /    \     /  s   \     /      \
--  -+ 2,-2 +---+  2,0   +---+ 2,2    +  | row  0
--    \    /2,-1 \      /2,1  \      /   |
--     +--+       +----+       +----+  --+
--
-- Rules are:
--   vertical columns are aligned: moving N/S is just changing the row index
--   horizontal rows are offset:
--     if the current column is even, then sw and se are on the same row (i.e translate (0,-1) and (0,1))
--                                    and nw and ne are on the previous row (i.e translate (-1,-1) and (-1,1))
--     if the current column is odd, then sw and se are on the next row (i.e translate (1,-1) and (1,1))
--                                    and nw and ne are on the same row (i.e translate (0,-1) and (0,1))
--
-- some distances to origin:
--   (0,0)  = 0 |
--   (-1,0) = 1 |
--   (1, 0) = 1 | <- manhattan
--   (2, 0) = 2 |
--   (2, 1) = 3 |
--   -----------+
--   (2, 2) = 3 | <- not manhattan
--   (1,2)  = 2 |
--   (-1,-1) = 1|
--

import           Control.Monad
import           Data.Attoparsec.Text    (Parser, parseOnly)
import qualified Data.Attoparsec.Text    as Atto
import           Data.Foldable           (foldl')
import           Data.Ord
import           Data.Text               (Text)
import qualified Data.Text               as Text
import qualified Data.Text.IO            as Text
import           System.Environment
import           System.Exit
import           Test.Hspec
import Test.QuickCheck
import           Text.Parser.Char
import           Text.Parser.Combinators (choice, sepBy1)
import           Text.Printf

data Coord = Coord { _x, _y :: Int } deriving (Show, Eq)

data Direction = NW | N | NE | SE | S | SW
               deriving (Show, Eq, Enum)

instance Arbitrary Direction where
  arbitrary = elements [NW .. SW]

instance Arbitrary Coord where
  arbitrary = Coord <$> arbitrary <*> arbitrary

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["pt1"]  -> run distanceAfter
    ["pt2"]  -> run maxDistanceReached
    ["test"] -> withArgs [] (hspec spec)
    _        -> die "bad arguments. Expected pt1, pt2, or test"
  where
    run f = Text.getContents >>= either die (print . f) . parseInput

parseInput :: Text -> Either String [Direction]
parseInput = parseOnly inputP

inputP :: Parser [Direction]
inputP = directionP `sepBy1` char ','

directionP :: Parser Direction
directionP = choice [NW <$ string "nw"
                    ,NE <$ string "ne"
                    ,SW <$ string "sw"
                    ,SE <$ string "se"
                    ,N <$ string "n"
                    ,S <$ string "s"
                    ]

move :: Coord -> Direction -> Coord
move loc = \case
  N -> translate (-1,0) loc
  S -> translate (1, 0) loc
  NW | even (_x loc) -> translate (-1,-1) loc
     | otherwise     -> translate (0 ,-1) loc
  NE | even (_x loc) -> translate (-1,1) loc
     | otherwise     -> translate (0, 1) loc
  SW | even (_x loc) -> translate (0,-1) loc
     | otherwise     -> translate (1,-1) loc
  SE | even (_x loc) -> translate (0,1) loc
     | otherwise     -> translate (1,1) loc

translate :: (Int, Int) -> Coord -> Coord
translate (dy,dx) (Coord x y) = Coord (dx + x) (dy + y)

hexDistance :: Coord -> Coord -> Int
hexDistance origin = go 0
  where go i loc = case (comparing _x origin loc, comparing _y origin loc) of
                     (EQ, EQ) -> i
                     (EQ, _)  -> i + abs (_y origin - _y loc)
                     (_, EQ)  -> i + abs (_x origin - _x loc)
                     (LT, LT) -> go (i + 1) (move loc NW)
                     (LT, GT) -> go (i + 1) (move loc SW)
                     (GT, LT) -> go (i + 1) (move loc NE)
                     (GT, GT) -> go (i + 1) (move loc SE)

distanceAfter :: [Direction] -> Int
distanceAfter = hexDistance origin . foldl' move origin

maxDistanceReached :: [Direction] -> Int
maxDistanceReached = maximum
                   . fmap (hexDistance origin)
                   . scanl move origin

origin :: Coord
origin = Coord 0 0

spec :: Spec
spec = do
  describe "maxDistanceReached" $ do
    it "can work out the furthest point" $ do
      maxDistanceReached [NE,SE,S,S,SW,NW,N,N] `shouldBe` 3
  describe "distanceAfter" $ do
    it "knows when we have gone in circles" $ do
      distanceAfter [NE,SE,S,S,SW,NW,N,N] `shouldBe` 0
    it "can solve example 1" $ do
      distanceAfter [NE,NE,NE] `shouldBe` 3
    it "can solve example 2" $ do
      distanceAfter [NE,NE,SW,SW] `shouldBe` 0
    it "can solve example 3" $ do
      distanceAfter [NE,NE,S,S] `shouldBe` 2
    it "can solve example 4" $ do
      distanceAfter [SE,SW,SE,SW,SW] `shouldBe` 3
  describe "parseInput" $ do
    it "can parse ex. path 1" $ do
      parseInput "ne,ne,ne" `shouldBe` pure [NE,NE,NE]
    it "can parse ex. path 2" $ do
      parseInput "ne,ne,sw,sw" `shouldBe` pure [NE,NE,SW,SW]
    it "can parse ex. path 3" $ do
      parseInput "ne,ne,s,s" `shouldBe` pure [NE,NE,S,S]
    it "can parse ex. path 4" $ do
      parseInput "se,sw,se,sw,sw" `shouldBe` pure [SE,SW,SE,SW,SW]
  describe "move" $ do
    it "results in a different point" $
      property $ \dir point -> move point dir /= point
    describe "cycles" $ do
      let cycles = [ -- clockwise
                    [N,SE,SW]
                   ,[NE,S,NW]
                   ,[SE,SW,N]
                   ,[S,NW,NE]
                   ,[SW,N,SE]
                   ,[NW,NE,S]
                   -- anti-clockwise
                   ,[N,SW,SE]
                   ,[NE,NW,S]
                   ,[SE,N,SW]
                   ,[S,NE,NW]
                   ,[SW,SE,N]
                   ,[NW,S,NE]
                   ]
      it "knows that moving a point in a cycle ends up at that same point" $ do
        property $ forAll (elements cycles) $ \cycle point ->
          foldl' move point cycle == point
      it "knows that failing to complete a cycle does not come home" $ do
        property $ forAll (elements cycles) $ \cycle point ->
          foldl' move point (take 2 cycle) /= point
  describe "hexDistance" $ do
    let origin = Coord 0 0
        table = [((0,0), 0)
                ,((-1,0),1)
                ,((1, 0),1)
                ,((2, 0),2)
                ,((2, 1),3)
                ,((2,-1),3)
                ,((-2,-1),2)
                ,((2, 2),3)
                ,((1,2),2)
                ,((-1,-1),1)
                ]
    forM_ table $ \((y,x), expected) -> do
      it (printf "Knows the distance from {x: %d, y: %d}" x y) $ do
        hexDistance origin (Coord x y) `shouldBe` expected
    it "is commutative" $ do
      property $ \a b -> hexDistance a b == hexDistance b a
    it "knows that any straight line is the shortest path" $
      property $ forAll (arbitrary `suchThat` (>= 0)) $ \n dir point ->
        let point' = foldl' move point (replicate n dir)
         in hexDistance point point' == n

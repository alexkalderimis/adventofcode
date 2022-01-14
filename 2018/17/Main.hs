{-# LANGUAGE OverloadedStrings #-}

module Main (main, spec) where

import           Control.Applicative
import qualified Data.Array              as A
import           Data.Attoparsec.Text    (Parser, parseOnly)
import           Data.Bool
import           Data.Foldable           (foldl')
import           Data.List.Extra         (find, unfoldr)
import qualified Data.HashSet            as S
import           Data.Text               (Text)
import qualified Data.Text               as Text
import qualified Data.Text.IO            as Text
import           System.Environment
import           System.Exit
import           Text.Parser.Char
import           Text.Parser.Combinators (between, sepBy1)
import           Text.Printf
import           Test.QuickCheck (counterexample)

import Elves hiding (left, right)
import Elves.Advent
import Elves.Coord.Strict
import Elves.Coord (translate)
import qualified Elves.StrictGrid as G

type Coord = Coordinate

type Section = (Coord, Coord)

data Terrain = Sand | Clay deriving (Show, Eq)

type Scan = A.Array Coord Terrain

data Water = Water
  { fallingWater :: !(S.HashSet Coordinate)
  , standingWater :: !(S.HashSet Coordinate)
  } deriving (Show, Eq)

type WaterSource = Coordinate

main :: IO ()
main = day 17 parseInput pt1 pt2 spec
  where
    -- pt1 = runAndShow
    pt1 = printf "Total water: %d\n" . totalWater . water
    pt2 = printf "Standing water: %d\n" . permaWater . water

    water = fillAll . buildScan

spec = do
  let Right readings = parseOnly parseInput exampleInput
      scan = buildScan readings
      water = fillAll scan

  it "solves pt1" . counterexample (showScan water scan) $ do
    totalWater water `shouldBe` 57

  it "solves pt2" . counterexample (showScan (water { fallingWater = S.empty }) scan) $ do
    permaWater water `shouldBe` 29

  context "source falls onto a column" $ do
    let input = Text.unlines ["x=500, y=5..8"
                             ,"x=502, y=5..8"
                             ,"x=495, y=2..10"
                             ,"x=505, y=2..10"
                             ,"y=9, x=500..502"
                             ,"y=11, x=495..505"
                             ]
    let Right readings = parseOnly parseInput input
        scan = buildScan readings
        water = fillAll scan

    it "solves pt1" . counterexample (showScan water scan) $ do
      totalWater water `shouldBe` 70

    it "solves pt2" . counterexample (showScan (water { fallingWater = S.empty }) scan) $ do
      permaWater water `shouldBe` 70

exampleInput = Text.unlines
  ["x=495, y=2..7"
  ,"y=7, x=495..501"
  ,"x=501, y=3..7"
  ,"x=498, y=2..4"
  ,"x=506, y=1..2"
  ,"x=498, y=10..13"
  ,"x=504, y=10..13"
  ,"y=13, x=498..504"
  ]

-- input parsing
parseInput :: Parser [Section]
parseInput = (verticalBlockP <|> horizontalBlockP) `sepBy1` newline

intP :: Parser Int
intP = read <$> some digit

totalWater :: Water -> Int
totalWater w = permaWater w + (S.size (fallingWater w) - 1)

permaWater :: Water -> Int
permaWater = S.size . standingWater

verticalBlockP, horizontalBlockP :: Parser Section

verticalBlockP = do
  x <- Col <$> coord 'x'
  string ", "
  y <- Row <$> coord 'y'
  string ".."
  y' <- Row <$> intP
  return (Coord y x, Coord y' x)

horizontalBlockP = do
  y <- Row <$>coord 'y'
  string ", "
  x <- Col <$> coord 'x'
  string ".."
  x' <- Col <$> intP
  return (Coord y x, Coord y x')

coord :: Char -> Parser Int
coord name = char name >> char '=' >> intP

-- build the scan table from the inputs
buildScan :: [Section] -> Scan
buildScan readings =
  let miny = minimum $ fmap (row . fst) readings
      maxy = maximum $ fmap (row . snd) readings
      minx = minimum $ fmap (col . fst) readings
      maxx = maximum $ fmap (col . snd) readings
      clay = S.fromList (readings >>= A.range)
      bs = (Coord miny (minx - 1), Coord maxy (maxx + 1))
   in A.array bs [ (loc, bool Sand Clay (S.member loc clay)) | loc <- A.range bs ]

showScan :: Water -> Scan -> String
showScan water scan
  = G.draw $ fmap solids scan
           A.// waterTiles
  where
    waterTiles = onScan $  [ (c, '|') | c <- S.toList (fallingWater water) ]
                        <> [ (c, '~') | c <- S.toList (standingWater water) ]

    solids Sand = '.'
    solids Clay = '#'

    onScan = let bs = A.bounds scan in filter (A.inRange bs . fst)

-- useful for exploring stages
runTo n = do
   let (Right readings) = parseOnly parseInput exampleInput
   let (_, scan) = scanAndFill readings
   let w = applyN n (fill scan) (spring scan)
   putStrLn (showScan w scan)

scanAndFill readings =
   let scan = buildScan readings
       w = fillAll scan
    in (w, scan)

-- actually run the input and present the results
run = mapM_ runAndShow . parseOnly parseInput

runAndShow readings = do
   let (w, scan) = scanAndFill readings
   putStrLn (showScan w scan)
   printf "Total water: %d\n"  (totalWater w)
   printf "Standing water: %d\n" (permaWater w)

spring scan = let y = row . fst $ A.bounds scan
              in Water { fallingWater = S.singleton (Coord (pred y) 500)
                       , standingWater = S.empty
                       }

fillAll :: Scan -> Water
fillAll scan = let src = spring scan
                   ws = iterate (fill scan) src
                in fst . fromJust . find (uncurry (==)) $ zip ws (tail ws)

fill :: Scan -> Water -> Water
fill scan water = Water { fallingWater = S.difference
                                          (S.union newFalling (fallingWater water))
                                          newStanding
                        , standingWater = S.union (standingWater water) newStanding
                        }
  where
    -- bunch of useful predicates
    inScan            = A.inRange (A.bounds scan)
    terrainIs t       = safeTest ((t ==) . (scan A.!))
    clay = terrainIs Clay
    sand = terrainIs Sand
    standing loc   = S.member loc (standingWater water)

    safeTest p        = inScan <&&> p
    passable          = sand   <&&> not . standing
    impassible        = clay   <||> standing
  
    falling  = passable . below
    spilling = impassible . below <&&> (passable . right <||> passable . left)
    filling  = impassible . below

    srcs           = S.toList $ fallingWater water
    newFalling     = S.fromList ((filter falling srcs >>= belows) <> (filter spilling srcs >>= spills))
    newStanding    = S.fromList (filter filling srcs >>= fillBasin)

    spills pos | impassible (below pos) = lefts pos <> rights pos <> edges pos
    spills _ = []
    edges pos = filter passable [left pos, right pos]

    belows = follow below passable
    lefts  = follow left  (passable <&&> impassible . below)
    rights = follow right (passable <&&> impassible . below)

    follow dir ok loc = let loc' = dir loc
                         in if ok loc' then loc' : follow dir ok loc' else []

    fillBasin pos = findBasin pos >>= A.range

    baseOfBasin :: Coord -> Maybe (Coord, Coord)
    baseOfBasin pos = (,) <$> bob' left pos <*> bob' right pos

    bob' f pos = do
      guard (sand pos && impassible (below pos))
      let pos' = f pos
      if clay pos' then pure pos else bob' f pos'

    findBasin :: Coord -> [(Coord, Coord)]
    findBasin = let validLevel (lb,ub) = clay (left lb) && clay (right ub) && all sand (A.range (lb,ub))
                    nextLevel  (lb,ub) = (above lb, above ub)
                in foldMap (takeWhile validLevel . iterate nextLevel) . baseOfBasin


left  = translate (Coord 0 (-1))
right = translate (Coord 0 1)
below = translate (Coord 1 0)
above = translate (Coord (-1) 0)

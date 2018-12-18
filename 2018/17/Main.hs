{-# LANGUAGE TupleSections #-}
{-# LANGUAGE OverloadedStrings #-}

import           Control.Applicative
import qualified Data.Array              as A
import           Data.Attoparsec.Text    (Parser, parseOnly)
import           Data.Bool
import           Data.Foldable           (foldl')
import           Data.List (unfoldr)
import qualified Data.Map.Strict         as M
import qualified Data.Set                as S
import           Data.Text               (Text)
import qualified Data.Text               as Text
import qualified Data.Text.IO            as Text
import           System.Environment
import           System.Exit
import           Text.Parser.Char
import           Text.Parser.Combinators (between, sepBy1)
import Text.Printf

type Coord = (Int, Int)

type Section = (Coord, Coord)

data Terrain = Sand | Clay deriving (Show, Eq)

type Scan = A.Array Coord Terrain

data Water = Standing | Falling deriving (Eq, Show)

type WaterOverlay = M.Map Coord Water

main :: IO ()
main = Text.getContents >>= run

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

translate (dy,dx) (y,x) = (y + dy, x + dx)

-- input parsing
parseInput :: Parser [Section]
parseInput = (verticalBlockP <|> horizontalBlockP) `sepBy1` newline

intP :: Parser Int
intP = read <$> some digit

verticalBlockP, horizontalBlockP :: Parser Section

verticalBlockP = do
  x <- coord 'x'
  string ", "
  y <- coord 'y'
  string ".."
  y' <- intP
  return ((y,x),(y',x))

horizontalBlockP = do
  y <- coord 'y'
  string ", "
  x <- coord 'x'
  string ".."
  x' <- intP
  return ((y,x),(y,x'))

coord :: Char -> Parser Int
coord name = char name >> char '=' >> intP

-- build the scan table from the inputs
buildScan :: [Section] -> Scan
buildScan readings =
  let miny = minimum $ fmap (fst . fst) readings
      maxy = maximum $ fmap (fst . snd) readings
      minx = minimum $ fmap (snd . fst) readings
      maxx = maximum $ fmap (snd . snd) readings
      clay = foldl' (foldr S.insert) mempty (A.range <$> readings)
      bs = ((miny,minx - 1),(maxy,maxx + 1))
   in A.array bs [(loc, bool Sand Clay (S.member loc clay))
                                         | loc <- A.range bs]

showScan :: WaterOverlay -> Scan -> String
showScan water scan = unlines [line y | y <- [0 .. maxy]]
  where
    ((miny,minx),(maxy,maxx)) = A.bounds scan
    line y = printf "%02d " y
           ++ [cell (y,x) | x <- [pred minx .. succ maxx]]
    cell (0,500) = '+'
    cell loc | Just w <- M.lookup loc water = case w of Standing -> '~'
                                                        Falling -> '|'
    cell loc | not (A.inRange (A.bounds scan) loc) = '.'
    cell loc = case scan A.! loc of
                 Sand -> '.'
                 Clay -> '#'

-- useful for exploring stages
runTo n = do
   let (Right readings) = parseOnly parseInput exampleInput
   let scan = buildScan readings
   let src = (spring $ fst $ fst $ A.bounds scan)
   let w = iterate (fill scan) src !! n
   putStrLn (showScan w scan)

-- actually run the input and present the results
run input = do
   let (Right readings) = parseOnly parseInput input
   let scan = buildScan readings
   let src = (spring $ fst $ fst $ A.bounds scan)
   let w = fillAll scan src
   putStrLn (showScan w scan)
   printf "Total water: %d\n"  (M.size w - M.size src)
   printf "Standing water: %d\n" (M.size $ M.filter (== Standing) w)

spring y = M.singleton (pred y,500) Falling

fillAll :: Scan -> WaterOverlay -> WaterOverlay
fillAll scan water = let ws = iterate (fill scan) water
                      in fst . head . dropWhile (uncurry (/=)) $ zip ws (tail ws)

fill :: Scan -> WaterOverlay -> WaterOverlay
fill scan water = additions <> water
  where
    additions = newStanding <> spills <> newFalling
    -- bunch of useful predicates
    inScan            = A.inRange (A.bounds scan)
    terrainIs t       = ((== t) . (scan A.!))
    safeTest p        = (&&) <$> inScan <*> p
    passable          = safeTest ((&&) <$> terrainIs Sand <*> not . standingWater)
    impassible        = safeTest ((||) <$> terrainIs Clay <*> standingWater)
    standingWater loc = maybe False (== Standing) (M.lookup loc water)
    spillovers pos    = filter passable [left pos, right pos]

    srcs           = [pos | (pos, Falling) <- M.toList water]
    newFalling     = mkWater Falling  . filter passable $ fmap below srcs
    spills         = mkWater Falling  $ filter (impassible . below) srcs >>= spillovers
    newStanding    = mkWater Standing $ srcs >>= fillBasin

    mkWater w = M.fromList . fmap (,w)

    fillBasin = maybe [] A.range . bucket
    bucket pos = if impassible (below pos)
                    then gob pos pos
                    else Nothing

    gob l r = (,) <$> gob' left l <*> gob' right r
    gob' f pos = let pos' = f pos
                 in if inScan pos' && impassible (below pos)
                       then if terrainIs Clay pos' then Just pos
                                                   else gob' f pos'
                       else Nothing


left = translate (0, -1)
right = translate (0, 1)
below = translate (1,0)

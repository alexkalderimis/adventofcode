{-# LANGUAGE OverloadedStrings #-}

import           Control.Applicative
import           Control.Lens                (each, (%~), (&))
import           Control.Monad
import           Control.Parallel.Strategies
import           Data.Attoparsec.Text        (Parser, parseOnly)
import qualified Data.List.Extra             as L
import           Data.Ord
import qualified Data.Set                    as S
import qualified Data.Text                   as Text
import qualified Data.Text.IO                as Text
import           System.Exit
import           Text.Parser.Char
import           Text.Parser.Combinators
import           Text.Parser.Token           (comma, integer')
import           Text.Printf

import Elves.Advent

type Distance = Int
type Coord = (Int,Int,Int)
type Region = (Coord, Coord)

data Nanobot = Nanobot { position       :: Coord
                       , signalStrength :: Distance
                       } deriving (Show, Ord, Eq)

main :: IO ()
main = day 23
           inputP
           (printf "Part one: %d\n" . S.size . partOne)
           (\ns -> do let (x,y,z) = partTwo ns
                      printf "Part two: %d,%d,%d\n" x y z
                      printf "distance to point: %d\n" (distance (0,0,0) (x,y,z)))
           (pure ())

partOne :: [Nanobot] -> S.Set Nanobot
partOne ns = let b = L.maximumBy (comparing signalStrength) ns
              in S.fromList $ filter ((<= signalStrength b) . distance (position b) . position) ns

-- There are too many co-ordinates (literally quadrillions of co-ordinates)
-- to scan exhaustively.
--   for the real input, should return:
--     Part two: 34078718,27904352,23778473
--     distance to point: 85761543
partTwo :: [Nanobot] -> Coord
partTwo ns = -- searchDividing ns bounds
             searchScaling 0 bounds ns
  where
        bounds = minmax (position <$> ns)

-- this works, but I'm not entirely sure why any more, since I can't
-- come up with a fast general solution.
--
-- Still a work in progress.
searchScaling :: Int -> (Coord, Coord) -> [Nanobot] -> Coord
searchScaling scale bounds ns
  | rs < 0 || rs > 100000 = searchScaling (scale + 1) bounds ns -- search space too big! Scale down first
  | otherwise             = -- Debug.traceShow (scale, bounds, scaledBounds) (
                              unscale scale . fst . L.maximumBy (comparing sort) $ counted
                            -- )
  where
    sort (c,n) = (n, Down (distance (0,0,0) c))
    rs = size scaledBounds
    f = 2 ^ scale -- choosing the scaling factor has a significant effect on results.
                    -- the exercise results are off by one when using 10, and correct when using 2
                    -- but the exampleHuge is only correct when using 2...
                    -- So this is at best a work in progress.
                    -- TODO: find a general solution!

    scaledBounds = (all3 div (fst bounds) (f,f,f), all3 div (snd bounds) (f,f,f))
    scaledBots = fmap (\b -> Nanobot (all3 div (position b) (f,f,f)) (signalStrength b `div` f)) ns
    nReachable c = length [() | b <- scaledBots , distance c (position b) <= signalStrength b]

    unscale 0 pos = pos
    unscale n (x,y,z) = -- Debug.traceShow ("FOUND", x,y,z) (
                          searchScaling (scale - 1) ((pred x * f, pred y * f, pred z * f), (succ x * f, succ y * f, succ z * f)) ns
                        -- )

    counted = let ((x,y,z),(x',y',z')) = scaledBounds
                  cs = [(a,b,c) | a <- [x .. x'], b <- [y .. y'], c <- [z .. z']]
               in zip cs $ parMap rdeepseq nReachable cs

searchDividing :: [Nanobot] -> Region -> Coord
searchDividing ns = go . pure
  where
    go rs
      | all (== 1) (size <$> rs) = L.minimumBy (comparing distanceHome) (fst <$> rs)
      | otherwise = go (keepBest $ scored (rs >>= divide))

    scored rs = zip rs (fmap (nReachable ns) rs)

    keepBest [] = error "no best"
    keepBest xs = let n = maximum (snd <$> xs)
                   in fmap fst $ filter ((== n) . snd) xs

    distanceHome = distance (0,0,0)

nReachable ns r = length [ () | (Nanobot p s) <- ns
                         , shortestPathTo p r <= s
                         ]

shortestPathTo :: Coord -> Region -> Distance
shortestPathTo c@(x,y,z) r@((minx,miny,minz),(maxx,maxy,maxz))
  | contains r c = 0
  | above r c    = abs (y - maxy)
  | below r c    = abs (miny - y)
  | infront r c  = abs (minz - z)
  | behind r c   = abs (z - maxz)
  | leftOf r c   = abs (minx - x)
  | rightOf r c  = abs (x - maxx)
  | otherwise    = minimum $ fmap (distance c) (corners r)

size :: Region -> Int
size ((x,y,z),(x',y',z')) = product [abs (x' - x) + 1, abs (y' - y) + 1, abs (z' - z) + 1]

divide :: Region -> [Region]
divide r | size r <= 8 && size r > 0 = L.nub $ L.sort [(c,c) | c <- corners r]
divide ((x,y,z),(x',y',z'))
  = [((xa,ya,za), (xb,yb,zb)) | (xa,xb) <- [(x,xmid),(xmid,x')]
                              , (ya,yb) <- [(y,ymid),(ymid,y')]
                              , (za,zb) <- [(z,zmid),(zmid,z')]
    ]
  where
    xmid = (x + x') `div` 2
    ymid = (y + y') `div` 2
    zmid = (z + z') `div` 2



above :: Region -> Coord -> Bool
above ((xmin,ymin,zmin),(xmax,ymax,zmax)) (x,y,z) = and [ xmin <= x
                                                        , xmax >= x
                                                        , zmin <= z
                                                        , zmax >= z
                                                        , ymax < y
                                                        ]
below :: Region -> Coord -> Bool
below ((xmin,ymin,zmin),(xmax,ymax,zmax)) (x,y,z) = and [ xmin <= x
                                                        , xmax >= x
                                                        , zmin <= z
                                                        , zmax >= z
                                                        , ymin > y
                                                        ]

infront :: Region -> Coord -> Bool
infront ((xmin,ymin,zmin),(xmax,ymax,zmax)) (x,y,z) = and [ xmin <= x
                                                          , xmax >= x
                                                          , ymin <= y
                                                          , ymax >= y
                                                          , zmin > z
                                                          ]
behind :: Region -> Coord -> Bool
behind ((xmin,ymin,zmin),(xmax,ymax,zmax)) (x,y,z) = and [ xmin <= x
                                                         , xmax >= x
                                                         , ymin <= y
                                                         , ymax >= y
                                                         , zmax < z
                                                         ]

leftOf :: Region -> Coord -> Bool
leftOf ((xmin,ymin,zmin),(xmax,ymax,zmax)) (x,y,z) = and [ xmin > x
                                                         , ymin <= y
                                                         , ymax >= y
                                                         , zmin <= z
                                                         , zmax >= z
                                                         ]
rightOf :: Region -> Coord -> Bool
rightOf ((xmin,ymin,zmin),(xmax,ymax,zmax)) (x,y,z) = and [xmax < x
                                                          ,ymin <= y
                                                          ,ymax >= y
                                                          ,zmin <= z
                                                          ,zmax >= z
                                                          ]

corners :: Region -> [Coord]
corners ((x,y,z),(x',y',z'))
  = L.nubOrd [(a,b,c) | a <- [x,x'], b <- [y,y'], c <- [z,z']]

contains :: Region -> Coord -> Bool
contains ((xmin,ymin,zmin),(xmax,ymax,zmax)) (x,y,z) = and [ xmin <= x, x <= xmax
                                                           , ymin <= y, y <= ymax
                                                           , zmin <= z, z <= zmax
                                                           ]

-- the coordinate with the best signal strength must lie within the bounding box of
-- all the bots themselves. This function builds the bounds for that bounding box in
-- a single pass over the co-ordinates.
minmax [] = error "minmax of empty list"
minmax (coord:coords) = L.foldl' (\(mins,maxes) c -> (all3 min c mins, all3 max c maxes)) (coord, coord) coords

all3 f (x,y,z) (x',y',z') = (f x x', f y y', f z z')

distance :: Coord -> Coord -> Distance
distance (x,y,z) (x',y',z') = d x x' + d y y' + d z z'
  where d a b = abs (b - a)

inputP :: Parser [Nanobot]
inputP = nanobot `sepBy1` newline
  where nanobot = Nanobot <$> pos <*> (string ", " *> radius)
        int = fromIntegral <$> integer'
        pos = string "pos=" *> ((,,) <$> (char '<' *> int)
                                     <*> (comma    *> int)
                                     <*> (comma    *> int <* char '>'))
        radius = string "r=" *> int

exampleInput = Text.unlines
  ["pos=<0,0,0>, r=4"
  ,"pos=<1,0,0>, r=1"
  ,"pos=<4,0,0>, r=3"
  ,"pos=<0,2,0>, r=1"
  ,"pos=<0,5,0>, r=3"
  ,"pos=<0,0,3>, r=1"
  ,"pos=<1,1,1>, r=1"
  ,"pos=<1,1,2>, r=1"
  ,"pos=<1,3,1>, r=1"
  ]

exampleTwo = Text.unlines
  ["pos=<10,12,12>, r=2"
  ,"pos=<12,14,12>, r=2"
  ,"pos=<16,12,12>, r=4"
  ,"pos=<14,14,14>, r=6"
  ,"pos=<50,50,50>, r=200"
  ,"pos=<10,10,10>, r=5"
  ]

exampleHuge = Text.unlines
  ["pos=<10000,12000,12000>, r=2000"
  ,"pos=<12000,14000,12000>, r=2000"
  ,"pos=<16000,12000,12000>, r=4000"
  ,"pos=<14000,14000,14000>, r=6000"
  ,"pos=<50000,50000,50000>, r=200000"
  ,"pos=<10000,10000,10000>, r=5000"
  ]

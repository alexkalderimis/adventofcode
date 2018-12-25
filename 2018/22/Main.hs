import           Control.Applicative
import qualified Data.Array              as A
import           Data.Attoparsec.Text    (Parser, parseOnly)
import qualified Data.List               as L
import qualified Data.Map.Strict         as M
import qualified Data.Set                as S
import qualified Data.Text.IO            as Text
import           System.Exit
import           Text.Parser.Char
import           Text.Printf

import qualified Elves.AStar             as AStar

data Terrain  = Rocky | Wet | Narrow deriving (Show, Eq, Enum)
data Equipped = Torch | ClimbingGear | Neither deriving (Show, Ord, Eq, Bounded, Enum)
type Coord    = (Int,Int)
type CaveMap  = A.Array Coord Terrain
type Depth    = Int

main :: IO ()
main = do
  ei <- parseOnly inputP <$> Text.getContents
  case ei of
    Left err -> die err
    Right (d,target) -> do
      printf "Region risk: %d\n" (riskLevelRegion (caveMap d target target))
      printf "Minimum rescue time: %d minutes\n" (rescueTime $ rescue d target)

riskLevel :: Terrain -> Int
riskLevel = fromEnum

rescueTime :: Maybe [(Equipped, Coord)] -> Int
rescueTime = maybe 0 totalPathCost
  where
   totalPathCost steps = sum
                       $ fmap (uncurry pathCost)
                       $ zip steps (tail steps)

rescue :: Depth -> Coord -> Maybe [(Equipped, Coord)]
rescue d target = (start :) <$> AStar.aStarOrd g distance estimate (== end) start
 where
   cave  = caveMap d target (2 * fst target, 2 * snd target)
   g     = neighbours cave target
   start = (Torch, (0,0))
   end   = (Torch, target)

   distance :: (Equipped, Coord) -> (Equipped, Coord) -> Double
   distance a b = fromIntegral (pathCost a b)

   estimate :: (Equipped, Coord) -> Double
   estimate (e,c) = AStar.euclideanDistance c target + if e == Torch then 0 else 7

-- useful for visualising routes as planned.
showRoute :: CaveMap -> [(Equipped,Coord)] -> String
showRoute cave route = unlines $ fmap row [0 .. maxy]
  where
    (maxx,maxy) = snd $ A.bounds cave
    row y = fmap (cell y) [0 .. maxx]
    equippedAt = M.fromList [(c,e) | (e,c) <- route]
    cell y x | Just e <- M.lookup (x,y) equippedAt
                 = case e of Torch        -> 'T'
                             ClimbingGear -> 'C'
                             Neither      -> 'N'
             | otherwise = case cave A.! (x,y) of
                             Rocky  -> '.'
                             Wet    -> '='
                             Narrow -> '|'


inputP :: Parser (Depth, Coord)
inputP = (,) <$> (string "depth: " *> int)
             <*> (newline >> string "target: " *> ((,) <$> int
                                                       <*> (char ',' >> int)))
  where int = read <$> some digit

caveMap :: Depth -> Coord -> Coord -> CaveMap
caveMap d target limit = fmap terrain geologicalIndices
  where
    bs = ((0,0), limit)
    geologicalIndices = A.listArray bs (gi <$> A.range bs)

    gi (0,0) = 0
    gi (x,0) = x * 16807
    gi (0,y) = y * 48271
    gi pos | pos == target = 0
    gi (x,y) = erosionLevel (geologicalIndices A.! (pred x,y))
             * erosionLevel (geologicalIndices A.! (x,pred y))

    erosionLevel idx = (d + idx) `mod` 20183
    terrain idx = toEnum (erosionLevel idx `mod` 3)

riskLevelRegion :: CaveMap -> Int
riskLevelRegion = sum . fmap riskLevel

pathCost :: (Equipped, Coord) -> (Equipped, Coord) -> Int
pathCost (e,(x,y)) (e',(x',y')) = ecost + abs (x' - x) + abs (y' - y)
  where
    ecost = if e == e' then 0 else 7

neighbours :: CaveMap -> Coord -> (Equipped, Coord) -> S.Set (Equipped, Coord)
neighbours cave target (e,c)
  = S.fromList [(e',c') | let moved      = (,) e . ($ c) <$> [up,down,left,right]
                        , let reequipped = flip (,) c <$> [minBound .. maxBound]
                        , (e',c') <- moved ++ reequipped
                        , (e',c') /= (e,c) -- different to current state
                        , A.inRange (A.bounds cave) c' -- within the cave
                        , canBeUsed e' (cave A.! c')
                        ]

canBeUsed :: Equipped -> Terrain -> Bool
canBeUsed Neither      Rocky  = False
canBeUsed Torch        Wet    = False
canBeUsed ClimbingGear Narrow = False
canBeUsed _ _                 = True

translate (dx,dy) (x,y) = (x + dx, y + dy)

up = translate    (0, -1)
down = translate  (0,  1)
left = translate  (-1, 0)
right = translate (1,  0)

exampleCave = unlines
 ["M=.|=.|.|=.|=|=." -- 00
 ,".|=|=|||..|.=..." -- 01
 ,".==|....||=..|==" -- 02
 ,"=.|....|.==.|==." -- 03
 ,"=|..==...=.|==.." -- 04
 ,"=||.=.=||=|=..|=" -- 05
 ,"|.=.===|||..=..|" -- 06
 ,"|..==||=.|==|===" -- 07
 ,".=..===..=|.|||." -- 08
 ,".======|||=|=.|=" -- 09
 ,".===|=|===T===||" -- 10
 ,"=|||...|==..|=.|" -- 11
 ,"=.=|=.=..=.||==|" -- 12
 ,"||=|=...|==.=|==" -- 13
 ,"|=.=||===.|||===" -- 14
 ,"||.|==.|.|.||=||" -- 15
-- 0123456789012345
 ]

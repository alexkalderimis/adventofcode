{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}

import           Control.Applicative
import qualified Data.Array              as A
import           Data.Maybe
import           Data.Foldable           (foldl')
import           Data.List               (unfoldr)
import qualified Data.Map.Strict         as M
import qualified Data.Set                as S
import           Data.Text               (Text)
import qualified Data.Text               as Text
import qualified Data.Text.IO            as Text
import           System.Environment
import           System.Exit
import           Text.Printf

data Item = OpenGround | Trees | Lumberyard deriving (Show, Eq, Ord)
type Coord = (Int,Int)

type LandScape = A.Array Coord Item

exampleInput = unlines
  [".#.#...|#."
  ,".....#|##|"
  ,".|..|...#."
  ,"..|#.....#"
  ,"#.#|||#|#|"
  ,"...#.||..."
  ,".|....|..."
  ,"||...#|.#|"
  ,"|.||||..|."
  ,"...#.|..|."
  ]

run n = do
  let land = parseInput exampleInput
  mapM_ (putStrLn . showLand) (take n (iterate evolve land))

showLand :: LandScape -> String
showLand land = unlines [row y | y <- [miny .. maxy]]
  where
    ((miny,minx),(maxy,maxx)) = A.bounds land
    row y = [cell (y,x) | x <- [minx .. maxx]]
    cell pos = case land A.! pos of OpenGround -> '.'
                                    Trees -> '|'
                                    Lumberyard -> '#'

parseInput :: String -> LandScape
parseInput str =
  let strs = lines str
      cells = [((y,x), cell c) | (y, row) <- zip [0 ..] strs
                               , (x, c) <- zip [0 ..] row
              ]
      bounds = ((0,0), (length strs - 1, maximum (fmap length strs) - 1))
   in A.array bounds cells
  where
    cell '.' = OpenGround
    cell '|' = Trees
    cell '#' = Lumberyard
    cell char = error $ "Unexpected character: " ++ [char]

evolve :: LandScape -> LandScape
evolve items = A.array (A.bounds items) (tick items <$> A.assocs items)

tick :: LandScape -> (Coord, Item) -> (Coord, Item)
tick items (pos, x) = (pos, tickWith (count neighbours) x)
  where
    neighbours = [items A.! p | shift <- [above, below, left, right, ul, ur, bl, br]
                              , let p = shift pos
                              , A.inRange (A.bounds items) p
                              ]
                    

tickWith :: M.Map Item Int -> Item -> Item
tickWith m OpenGround | num Trees m >= 3 = Trees
tickWith m Trees | num Lumberyard m >= 3 = Lumberyard
tickWith m Lumberyard = if num Lumberyard m >= 1 && num Trees m >= 1
                           then Lumberyard
                           else OpenGround
tickWith _ item = item

translate (dy,dx) (y,x) = (y + dy, x + dx)
left = translate (0, -1)
right = translate (0, 1)
below = translate (1,0)
above = translate (-1,0)
ul = left . above
ur = right . above
bl = left . below
br = right . below

count :: [Item] -> M.Map Item Int
count = M.fromListWith (+) . fmap (,1)

num :: Item -> M.Map Item Int -> Int
num = (fromMaybe 0 .) . M.lookup

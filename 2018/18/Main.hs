{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}

import           Control.Applicative
import           Control.Monad
import qualified Data.Array          as A
import           Data.Foldable       (foldl')
import           Data.List           (unfoldr)
import qualified Data.Map.Strict     as M
import           Data.Maybe
import qualified Data.Set            as S
import           System.Environment
import           System.Exit
import           Text.Printf
import           Text.Read

import           Options.Generic

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

data Options = Options
  { optN     :: Maybe String
  , optDebug :: Bool
  } deriving (Generic, Show)

instance ParseRecord Options

main :: IO ()
main = do
  opts <- getRecord "AOC Dec 18"
  n <- case optN opts of
        Just "pt1" -> pure 10
        Just "pt2" -> pure 1000000000
        Just  s    -> maybe (die $ "Bad argument : " ++ s) pure (readMaybe s)
  getContents >>= run (optDebug opts) n

run :: Bool -> Int -> String -> IO ()
run debug n input = do
  let land = looping evolve n (parseInput input)
  putStrLn (showLand land)
  printf "Resource value: %d\n" (resourceValue land)

looping :: Ord a => (a -> a) -> Int -> a -> a
looping f n x = go (M.singleton x n) n x
  where
    go _ 0 x = x
    go m n x =
      let x'   = f x
          loop = maybe 0 (loopSize n) (M.lookup x' m)
          n'   = n - (1 + loop)
      in go (M.insert x' n' m) n' x'
    loopSize n loopStart = let loopLen = loopStart - (n - 1)
                               loops = (n - 1) `div` loopLen
                            in loops * loopLen

resourceValue :: LandScape -> Int
resourceValue = uncurry (*) . foldl' translate (0,0) . fmap f . A.elems
  where
    f Trees      = (1,0)
    f Lumberyard = (0,1)
    f _          = (0,0)

showLand :: LandScape -> String
showLand land = unlines [row y | y <- [miny .. maxy]]
  where
    ((miny,minx),(maxy,maxx)) = A.bounds land
    row y = [cell (y,x) | x <- [minx .. maxx]]
    cell pos = case land A.! pos of OpenGround -> '.'
                                    Trees      -> '|'
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
    cell '.'  = OpenGround
    cell '|'  = Trees
    cell '#'  = Lumberyard
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

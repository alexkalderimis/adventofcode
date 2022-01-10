module Support.TilingSupport where

import qualified Data.List as L
import Control.Lens.Combinators (none)
import Control.Arrow (second)
import qualified Data.Array as A
import           Data.Array ((//))
import qualified Data.Map as M

import Test.QuickCheck hiding (scale, NonZero, within)

import qualified Elves.StrictGrid as G
import Elves.Coord (Bounds, overlaps)
import Elves.Cartesian
import Elves.Tiling

type Depicted = A.Array G.Coord Char
type Pixel = (G.Coord, Char)
data BoxSpec = Box { fillChar :: Maybe Char, vEdgeChar :: Char, hEdgeChar :: Char }

data NeighbourInput = NeighbourInput (Bounds Location) (Bounds Location)
  deriving (Show, Eq)

data TileInput = TileInput (Bounds Location) [Bounds Location]
  deriving (Show, Eq)

splitOf :: Bounds Location -> Gen (Either YPos XPos)
splitOf r = oneof [ySplitOf r, xSplitOf r]

ySplitOf, xSplitOf :: Bounds Location -> Gen (Either YPos XPos)
ySplitOf (lb, ub) = Left <$> chooseInt (y lb, y ub)
xSplitOf (lb, ub) = Right <$> chooseInt (x lb, x ub)

arbBox minx maxx miny maxy = do
  tly <- chooseInt (miny, maxy)
  tlx <- chooseInt (minx, maxx)
  bly <- chooseInt (tly, maxy)
  blx <- chooseInt (tlx, maxx)
  pure ((tly,tlx),(bly,blx))

instance Arbitrary NeighbourInput where
  arbitrary = do
    outer <- arbBox (-10) 50 (-10) 50
    inner <- arbBox (x $ lb outer) (x $ ub outer) (y $ lb outer) (y $ ub outer)

    return (NeighbourInput outer inner)

instance Arbitrary TileInput where
  arbitrary = do
    outer <- arbBox 0 1000 0 1000
    let inner = arbBox (x $ lb outer) (x $ ub outer) (y $ lb outer) (y $ ub outer)
    is <- listOf1 inner
    pure (TileInput outer (dedup is))
    where
      dedup = L.nubBy overlaps

-- helps to eyeball tiling and compactify issues
runExample = do
  let outer = ((0,0),(26,24))
      is = trim outer <$>
           [((0,16),(3,24))
           ,((0,0),(4,6))
           ,((4,18),(10,24))
           ,((12,0),(20,7))
           ,((16,12),(22,18))
           ,((24,4),(30,18))
           ]
      ts = tiles outer is

  putStrLn "INTERSECTIONS ==================="
  putStrLn (G.draw $ depict outer is [])
  putStrLn "TILES ==================="
  putStrLn (G.draw $ depict outer is (compactify ts))

mainBox         = Box Nothing '│' '─'
intersectionBox = Box (Just '/') '│' '─'
tileBox         = Box (Just '.') '┊' '┈'

depict :: Bounds Location -> [Bounds Location] -> [Bounds Location] -> Depicted
depict outer intersections tiles
  = depictOverlays tileBox tiles
  . depictOverlays intersectionBox intersections
  $ depictBase outer

depictBase :: Bounds Location -> Depicted
depictBase outer = depictOverlays mainBox [outer]
                 $ A.listArray (coords outer) (repeat ' ')

depictOverlays :: BoxSpec -> [Bounds Location] -> Depicted -> Depicted
depictOverlays _ [] board = board
depictOverlays spec regions board
  = board // fill
          // vedges
          // hedges
          // cnrMarkers
  where
    fill       = case fillChar spec of
                   Nothing -> []
                   Just chr -> [ (coord loc, chr)  | loc <- regions >>= A.range ]
    cnrMarkers = mconcat [ edgeSet "TL" '┌' , edgeSet "TR" '┐'
                         , edgeSet "BL" '└' , edgeSet "BR" '┘'
                         ]

    vedges     = edgeSet "L" (vEdgeChar spec) <> edgeSet "R" (vEdgeChar spec)
    hedges     = edgeSet "B" (hEdgeChar spec) <> edgeSet "T" (hEdgeChar spec)

    edgeSet k chr = [ (coord loc, chr) | bs <- edgeSets M.! k, loc <- A.range bs ]
    edgeSets = M.fromListWith (<>) (regions >>= \r -> fmap (second pure) $ namedNeighbours r (interior r))

coord loc = G.Coord (G.Row $ y loc) (G.Col $ x loc)
coords (lb, ub) = (coord lb, coord ub)
interior (lb, ub) = (down (right lb), up (left ub))

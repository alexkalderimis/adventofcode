-- Two dimenstional tiling
module Elves.Tiling where

import Data.Ord
import qualified Data.List as L
import Data.Ix
import Control.Arrow ((&&&))
import Control.Applicative

import Elves.Coord hiding (corners)
import Elves.Cartesian

type XPos = Int
type YPos = Int

data Limit = Limit
  { limitDir :: Direction  -- this is a limit for all points DIR of this
  , limitRange :: Bounds Int -- this is a limit for all values in this range
  , limitValue :: Int -- this is the limit
  } deriving (Show, Eq)

data LimitStyle = Inner | Outer deriving (Show, Eq)

limits :: LimitStyle -> Bounds Location -> [Limit]
limits style r =
  [Limit East  vrange (if style == Inner then leftEdge r  else rightEdge r + 1)
  ,Limit West  vrange (if style == Inner then rightEdge r else leftEdge r - 1)
  ,Limit South hrange (if style == Inner then topEdge r   else btmEdge r + 1)
  ,Limit North hrange (if style == Inner then btmEdge r   else topEdge r - 1)
  ]
  where hrange = (leftEdge r, rightEdge r)
        vrange = (topEdge r, btmEdge r)

-- given a region, and a set of intersections with that region, return a set
-- of regions which cover the area not intersected with.
--
-- i.e. if the region is
--  
--  +----------+
--  |          |
--  |          |
--  |          |
--  +----------+
--
--  And it has intersections (//):
--  
--  +----------+
--  |//|       |
--  ---+  +----|
--  |     |////|
--  +----------+
--
--  Then we return tiles such as:
--
--     --------+
--     |       |
--  +--+-------+
--  |     |     
--  +-----+     
--  
--  tiling may produce a large number of small fragments to tessalate the space.
--  If you are OK with many small regions, then that's fine, but if you prefer
--  to have fewer, compact regions, then call `compactify` on the resulting set of
--  regions
tiles :: Bounds Location -> [Bounds Location] -> [Bounds Location]
tiles outer [] = [outer]
tiles outer (t:ts)
  | overlaps outer t = neighbours outer t >>= \outer' -> tiles outer' ts
  | otherwise        = tiles outer ts

splitX :: XPos -> Bounds Location -> [Bounds Location]
splitX xpos (lb, ub)
  | inRange (x lb, x ub) xpos = [(lb, (y ub, xpos)), ((y lb, xpos + 1), ub)]
  | otherwise = [(lb, ub)]

splitY :: YPos -> Bounds Location -> [Bounds Location]
splitY ypos (lb, ub)
  | inRange (y lb, y ub) ypos = [(lb, (ypos, x ub)), ((ypos + 1, x lb), ub)]
  | otherwise = [(lb, ub)]

splitRegion :: Either YPos XPos -> Bounds Location -> [Bounds Location]
splitRegion (Left ypos) = splitY ypos
splitRegion (Right xpos) = splitX xpos

splits :: Bounds Location -> [Either YPos XPos] -> [Bounds Location]
splits r = L.foldl' (\rs s -> rs >>= splitRegion s) [r]

-- try to find neighbouring regions which can be joined, first looking LR, and then TB
compactify :: [Bounds Location] -> [Bounds Location]
compactify = joinTB . joinLR . L.sortBy (comparing lb)
  where
    joinLR [] = []
    joinLR [x] = [x]
    joinLR (a:b:xs) =
      let tra = topRight a
          tlb = topLeft b
          bra = btmRight a
          blb = btmLeft b
       in if (tra == left tlb) && (bra == left blb)
             then joinLR ((lb a, ub b) : xs)
             else a : joinLR (b : xs)

    joinTB [] = []
    joinTB [x] = [x]
    joinTB (a:xs) = let bla = btmLeft a
                        bra = btmRight a
                        joins r = topLeft r == down bla && topRight r == down bra
                     in case L.partition joins xs of
                          ((b:bs), rst) -> joinTB ((lb a, ub b) : bs <> rst)
                          (_, rst) -> a : joinTB rst

-- preconditions: lims is sorted ascending, and contains a value greater than
-- any input
downThenRight lims tl =
  let bly = head [lim | Limit North r lim <- lims
                      , inRange r (x tl)
                      , (y tl) <= lim]
      brx = head [lim | Limit West  r lim <- lims
                      , overlaps r (y tl, bly)
                      , (x tl) <= lim]
      br = (bly, brx)
   in extendLeft lims (tl,br)

-- preconditions: lims is sorted ascending, and contains a value greater than
-- any input
rightThenDown lims tl =
  let trx = head [lim | Limit West  r lim <- lims
                      , inRange r (y tl)
                      , (x tl) <= lim]
      bry = head [lim | Limit North r lim <- lims
                      , overlaps r (x tl, trx)
                      , (y tl) <= lim]
      br = (bry, trx)
   in extendUp lims (tl, br)

extendLeft :: [Limit] -> Bounds Location -> Bounds Location
extendLeft lims reg =
  let top = topEdge reg
      btm = btmEdge reg
      lhs = leftEdge reg
      tlx = L.maximum [lim | Limit East r lim <- lims
                           , overlaps r (top, btm)
                           , lim <= lhs]
   in ((top, tlx), ub reg)

extendUp :: [Limit] -> Bounds Location -> Bounds Location
extendUp lims reg =
  let top = topEdge reg
      lhs = leftEdge reg
      rhs = rightEdge reg
      tly = L.maximum [lim | Limit South r lim <- lims
                           , overlaps r (lhs, rhs)
                           , lim <= top]
   in ((tly, lhs), ub reg)

-- trim two overlapping bounds to their intersection
trim :: Bounds Location -> Bounds Location -> Bounds Location
trim a b = ((max (topEdge a) (topEdge b), max (leftEdge a) (leftEdge b))
           ,(min (btmEdge a) (btmEdge b), min (rightEdge a) (rightEdge b))
           )

-- returns the bounds of the eight neighbours: TL, L, BL, T, B, TR, R, BR
--
--   outer
--  +--------+-----------+--------+  -
--  |α       |β          |γ       |  | Top-margin
--  |  TL    |    T      |  TR    |  |
--  |      α'|         β'|      γ'|  |
--  +--------+-----------+--------+  -
--  |δ       |///////////|ε       |  | Inner-height
--  |   L    |//inner////|   R    |  |
--  |      δ'|///////////|      ε'|  |
--  +--------+-----------+--------+  -
--  |ζ       |η          |θ       |  | Bottom-margin
--  |  BL    |    B      |  BR    |  |
--  |      ζ'|         η'|      θ'|  |
--  +--------+-----------+--------+  -
--
--  |--------|-----------|--------|
--    Left     Inner       Right
--    margin   width       margin
--
-- Returned regions are all fully contained in the outer region.
neighbours :: Bounds Location -> Bounds Location -> [Bounds Location]
neighbours outer inner | outer == inner = []
neighbours outer inner | not (overlaps outer inner) = []
neighbours outer inner = fmap snd $ namedNeighbours outer inner

namedNeighbours :: Bounds Location -> Bounds Location -> [(String, Bounds Location)]
namedNeighbours outer overlay = concat [tl, cl, bl, tc, bc, tr, cr, br]
  where
    tl = aka "TL" [ (α,α') | leftMarginExists && topMarginExists ]
    tc = aka "T"  [ (β,β') | topMarginExists ]
    tr = aka "TR" [ (γ,γ') | topMarginExists && rightMarginExists ]

    cl = aka "L"  [ (δ,δ') | leftMarginExists ]
    cr = aka "R"  [ (ε,ε') | rightMarginExists ]

    bl = aka "BL" [ (ζ,ζ') | leftMarginExists && bottomMarginExists ]
    bc = aka "B"  [ (η,η') | bottomMarginExists ]
    br = aka "BR" [ (θ,θ') | bottomMarginExists && rightMarginExists ]

    leftMarginExists   = leftEdge outer  < leftEdge inner
    topMarginExists    = topEdge outer   < topEdge inner
    rightMarginExists  = rightEdge inner < rightEdge outer
    bottomMarginExists = btmEdge inner   < btmEdge outer

    inner = trim outer overlay

    aka name = fmap ((,) name)

    -- the points themselves
    α  = topLeft outer
    α' = up . left $ topLeft inner
    β  = (topEdge outer, leftEdge inner)
    β' = up (topRight inner)
    γ  = (topEdge outer, γεθ_x_min)
    γ' = (topEdge inner - 1, rightEdge outer)
    δ  = (topEdge inner, leftEdge outer)
    δ' = (btmEdge inner, αδζ_x_max)
    ε  = right (topRight inner)
    ε' = (btmEdge inner, rightEdge outer)
    ζ  = (ζηθ_y_min, leftEdge outer)
    ζ' = (btmEdge outer, αδζ_x_max)
    η  = (ζηθ_y_min, leftEdge inner)
    η' = (btmEdge outer, rightEdge inner)
    θ  = (ζηθ_y_min, γεθ_x_min)
    θ' = btmRight outer

    ζηθ_y_min = btmEdge inner + 1
    αδζ_x_max = leftEdge inner - 1
    γεθ_x_min = rightEdge inner + 1

-- Bunch of aliases for fst and snd to make life saner:

height :: Bounds Location -> Int
height = (+ 1) . liftA2 subtract btmEdge topEdge

width :: Bounds Location -> Int
width = (+ 1) . liftA2 subtract rightEdge leftEdge

corners :: Bounds Location -> [Location]
corners bs = fmap ($ bs) [topLeft, topRight, btmRight, btmLeft]

edges :: Bounds Location -> [Bounds Location]
edges bs = fmap (\(lb, ub) -> (lb bs, ub bs))
           [(topLeft, topRight), (topLeft, btmLeft), (btmLeft, btmRight), (topRight, btmRight)]

leftEdge :: Bounds Location -> XPos
leftEdge = x . lb

rightEdge :: Bounds Location -> XPos
rightEdge = x . ub

topEdge :: Bounds Location -> YPos
topEdge = y . lb

btmEdge :: Bounds Location -> YPos
btmEdge = y . ub

topLeft :: Bounds Location -> Location
topLeft = topEdge &&& leftEdge

topRight :: Bounds Location -> Location
topRight = topEdge &&& rightEdge

btmRight :: Bounds Location -> Location
btmRight = btmEdge &&& rightEdge

btmLeft :: Bounds Location -> Location
btmLeft = btmEdge &&& leftEdge

lb :: Bounds Location -> Location
lb = fst

ub :: Bounds Location -> Location
ub = snd

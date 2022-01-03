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

tiles :: Bounds Location -> [Bounds Location] -> [Bounds Location]
tiles outer [] = [outer]
tiles outer is = if null validStartPoints then [] else largestTile : otherTiles
  where
    largestTile = L.maximumBy (comparing rangeSize)
                              (validStartPoints >>= candidateTiles)
    otherTiles = neighbours outer largestTile >>= \n ->
                   tiles n (fmap (trim n) . filter (overlaps n) $ is)
    validStartPoints = filter valid (corners outer ++ intersectionStartPoints)
    candidateTiles p = [downThenRight lims p, rightThenDown lims p]
    lims    = L.sortBy (comparing limitValue)
              $ limits Inner outer ++ (limits Outer =<< is)
    valid p = inRange outer p && all (\i -> not (inRange i p)) is
    intersectionStartPoints = [right . topRight, down . btmLeft] <*> is

compactify :: [Bounds Location] -> [Bounds Location]
compactify = go . L.sortBy (comparing lb)
  where
    go [] = []
    go [x] = [x]
    go (a:b:xs) =
      let tra = topRight a
          tlb = topLeft b
          bra = btmRight a
          blb = btmLeft b
       in if (tra == left tlb) && (bra == left blb)
             then compactify ((lb a, ub b) : xs)
             else a : compactify (b : xs)

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
neighbours :: Bounds Location -> Bounds Location -> [Bounds Location]
neighbours outer inner = fmap snd (namedNeighbours outer inner)

namedNeighbours outer inner = concat [tl, l_, bl, t_, b_, tr, r_, br]
  where
    tl = fmap ((,) "TL") [ (α,α') | leftMarginExists && topMarginExists ]
    t_ = fmap ((,) "T")  [ (β,β') | topMarginExists ]
    tr = fmap ((,) "TR") [ (γ,γ') | topMarginExists && rightMarginExists ]
    l_ = fmap ((,) "L")  [ (δ,δ') | leftMarginExists ]
    r_ = fmap ((,) "R")  [ (ε,ε') | rightMarginExists ]
    bl = fmap ((,) "BL") [ (ζ,ζ') | leftMarginExists && bottomMarginExists ]
    b_ = fmap ((,) "B")  [ (η,η') | bottomMarginExists ]
    br = fmap ((,) "BR") [ (θ,θ') | bottomMarginExists && rightMarginExists ]

    leftMarginExists   = leftEdge outer  < leftEdge inner
    topMarginExists    = topEdge outer   < topEdge inner
    rightMarginExists  = rightEdge inner < rightEdge outer
    bottomMarginExists = btmEdge inner   < btmEdge outer

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

x :: Location -> Int
x = snd

y :: Location -> Int
y = fst

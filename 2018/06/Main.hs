{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Main(main) where

import           Control.Arrow (first)
import           Control.Monad.ST as ST
import           Data.Array.ST (STArray, freeze, newArray, writeArray, readArray)
import qualified Data.Array as A
import qualified Data.Array.ST as STA
import           Data.Char (toUpper)
import           Data.Maybe
import qualified Data.Text as T
import qualified Data.Ix as Ix
import qualified Data.List.Extra as L
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import           Data.Attoparsec.Text (string, decimal)
import           Control.Applicative.Combinators (sepBy1)
import           Text.Parser.Char (newline)

import Elves
import Elves.Advent
import Elves.Coord.Strict
import Elves.StrictGrid as G
import qualified Elves.CountMap as CM
import qualified Elves.Coord as C
import qualified Elves.RTree as RT
import           Elves.RTree (RTree)

-- This exercise has three solutions, playing with techniques and
-- looking for performance.
-- 
-- In order of performance we have:
--   * flood-fill, which iteratively fills the field with straight-line
--   neighbours. This makes use of localised safe mutable arrays for
--   efficiency. Most verbose, by a very long way.
--   * rtree, which uses a spatial index to look for nearest neighbours
--   efficiently. Relies heavily on inlining and specialization of Coord
--   class for performance.
--   * The naive approach for reference, which checks every point
--   for every input node, so O(n.m). Simplest to write, by a long way.
--
-- pt1-floodfill
-- -------------
-- <311,142>: 8219
-- -------------
-- 0.119373161s
--
-- pt1-rtree
-- ---------
-- <311,142>: 8219
-- ---------
-- 0.224373427s
--
-- pt1-naive
-- ---------
-- <311,142>: 8219
-- ---------
-- 0.425131356s
--
-- pt1-floodfill-large
-- -------------------
-- <-622,-284>: 36746
-- -------------------
-- 0.449390325s
--
-- pt1-rtree-large
-- ---------------
-- <-622,-284>: 36746
-- ---------------
-- 1.188995259s
--
-- pt1-naive-large
-- ---------------
-- <-622,-284>: 36746
-- ---------------
-- 3.82407528s
--
-- pt2
-- ---
-- SAFE ZONE SIZE:
-- 42036
-- ---
-- 0.020393087s

-- strict unpacked tuples
type Coord = Coordinate

type BoundingBox = (Coord, Coord)

type Pt1Answer = (Coord, CM.Total)

data Ownership = Unclaimed
               | Disputed
               | Owned {-# UNPACK #-} !Coord
               | Is {-# UNPACK #-} !Coord
               deriving (Show)

main :: IO ()
main = generalDay 6 (coordP `sepBy1` newline) spec
       [ ("pt1-floodfill", doFloodFill)
       , ("pt1-rtree", doRTree)
       , ("pt1-naive", doNaive)
       , ("pt1-floodfill-large", doFloodFill . bigField)
       , ("pt1-rtree-large", doRTree . bigField)
       , ("pt1-naive-large", doNaive . bigField)
       , ("pt2", doSafeZone)
       ]

coordP :: Parser Coord
coordP = do
  x <- decimal
  string ", "
  y <- decimal

  pure (Coord (Row y) (Col x))

boundingBox :: [Coord] -> BoundingBox
boundingBox = L.foldl1 C.expandB . fmap (\c -> (c,c))
    
owner :: (Applicative f, MonadPlus f) => Ownership -> f Coord
owner (Owned x) = pure x
owner (Is x)    = pure x
owner _         = mzero

claimed :: Ownership -> Bool
claimed Unclaimed = False
claimed _         = True

-- Safe zone impl. Pt2 of the exercise
safeZone :: Int -> [Coord] -> Int
safeZone limit cs = count ((< limit) . manhattanSum) (Ix.range $ boundingBox cs)
    where
      manhattanSum c = sum (manhattan c <$> cs)

-- General reusable components of the different approaches

-- Given a nearest-neighbour search function, produces the result
generalResult :: (Coord -> Maybe Coord) -> S.Set Coord -> BoundingBox -> Maybe (Coord, CM.Total)
generalResult uniqClosest points bb
  = listToMaybe
  . L.sortOn (Down . snd)
  . CM.counts
  . CM.fromList
  . filter notEscaped
  . mapMaybe uniqClosest
  $ Ix.range bb
  where
    notEscaped = flip S.notMember escaped
    escaped = escapees uniqClosest points bb

onEdge :: BoundingBox -> Coord -> Bool
onEdge (lb,ub) c = or [ col c == col lb
                      , col c == col ub
                      , row c == row lb
                      , row c == row ub
                      ]

-- a point escapes if: it is on the edge, or it can claim a corner
escapees :: (Coord -> Maybe Coord) -> S.Set Coord -> BoundingBox -> S.Set Coord
escapees uniqClosest points bb = S.filter (onEdge bb) points
                                 `S.union`
                                 S.fromList (mapMaybe uniqClosest $ C.corners bb)

-- Flood fill approach:

type Field = A.Array Coord Ownership

fieldResult :: Field -> Maybe Pt1Answer
fieldResult fld = let ps = S.fromList [ c | Is c <- A.elems fld ]
                   in generalResult uniqClosest ps (A.bounds fld)
    where 
      uniqClosest c = owner (fld A.! c)

drawField :: Field -> String
drawField fld = G.draw $ fmap pixel fld
  where
    names = M.fromList $ zip [ c | Is c <- A.elems fld ] ['a' .. ]
    pixel Unclaimed = ' '
    pixel Disputed  = '.'
    pixel (Is c)    = maybe '?' toUpper (M.lookup c names)
    pixel (Owned c) = fromMaybe '?' (M.lookup c names)

floodFill :: [Coord] -> Field
floodFill cs = do
  let bb = boundingBox cs

  runST $ do
    field <- newArray bb Unclaimed
    mapM_ (\c -> writeArray field c (Is c)) cs
    go field (M.fromList [(c, S.singleton c) | c <- cs])

  where
    go field frontier = do
      frontier' <- step field frontier
      if all S.null (M.elems frontier')
         then freeze field
         else go field frontier'

-- because we use manhattan distances, we only move in straight lines, and
-- we only consider 4 neighbours at each fill
neighbours :: Coord -> [Coord]
neighbours (Coord y x) = [                 Coord (pred y) x
                         ,Coord y (pred x)                 , Coord y (succ x)
                                          ,Coord (succ y) x
                         ]

countClaims :: Field -> M.Map Coord Int
countClaims = M.fromListWith (+) . (claimCount <=< A.elems)
  where
      claimCount o = (,1) <$> owner o

type Frontier = M.Map Coord (S.Set Coord)

step :: STArray s Coord Ownership -> Frontier -> ST s Frontier
step field frontier = do
  bs   <- STA.getBounds field
  wave <- M.traverseWithKey (advance bs) frontier
  let (uncontested, conflicts) = contest wave
  mapM_ writeClaims (M.toList uncontested)
  mapM_ writeConflict conflicts
  return wave

    where
      -- store claims and disputes in the field
      writeClaims (owner, claims) = forM_ claims $ \claim ->
        writeArray field claim (Owned owner)
      writeConflict c =
        writeArray field c Disputed
      -- a claim is uncontested if it is only claimed once
      contest wave =
        let newClaims = M.unionsWith (+) . fmap (M.fromSet (pure one)) $ M.elems wave
            uncontested = S.filter ((one ==) . (newClaims M.!))
         in (M.map uncontested wave, M.keysSet (M.filter (> one) newClaims))
      -- the next wave is all the straight-line neighbours of this frontier
      -- that are further from the group origin than the frontier
      nextWave bs src frontLine = do
        c <- S.toList frontLine
        n <- neighbours c
        guard (Ix.inRange bs n)
        guard (manhattan src n > manhattan src c)
        pure n
      onlyUnclaimed = filterM (fmap (not.claimed) . readArray field)
      advance bs capital frontLine =
        S.fromList <$> onlyUnclaimed (nextWave bs capital frontLine)
      one :: Int
      one = 1

--- The naive implementation.
-- This is not really slower than the RTree solution, when O2 is enabled.
-- However without optimisation or in ghci, it is much slower.
--
-- It is however much shorter though.

naive :: [Coord] -> Maybe Pt1Answer
naive cs = generalResult uniqClosest (S.fromList cs) bb
    where
      bb = boundingBox cs
      uniqClosest c = case listToMaybe $ L.groupSortOn (manhattan c) cs of
                        Just [a] -> pure a
                        _        -> mzero

treeResult :: RTree Coord Coord -> Maybe Pt1Answer
treeResult tree =
  let ps = S.fromList (fst <$> RT.keys tree)
   in RT.bounds tree >>= generalResult uniqClosest ps
  where
    uniqClosest c = case take 2 $ nearest tree c of
      [(c, d0), (_, d1)] | d0 == d1 -> mzero
      []                            -> mzero
      ((c, _) : _)                  -> pure c

-- nearest neighour search in the RTree
-- Returns the value and its distance
nearest :: RTree Coord Coord -> Coord -> [(Coord, Int)]
nearest t c = (RT.neighbour &&& RT.neighbourDistance) <$> RT.priorityOrder manhattan c t

spec = do
  let examplePoints = ["1, 1" -- a
                      ,"1, 6" -- b
                      ,"8, 3" -- c
                      ,"3, 4" -- d
                      ,"5, 5" -- e
                      ,"8, 9" -- f
                      ]
      Right cs = traverse (parseOnly coordP) examplePoints
      answer = (Coord 5 5, 17)

  describe "flood-fill" $ do
    let fld = floodFill cs
    it "gets the right answer" $ do
      fieldResult fld `shouldBe` Just answer

    it "can be rendered" $ do
      putStrLn (drawField fld)

  describe "naive" $ do
    it "gets the right answer" $ do
      naive cs `shouldBe` Just answer

  describe "rtree" $ do
    let t = RT.fromPoints (zip cs cs)

    it "gets the right answer" $ do
      treeResult t `shouldBe` Just answer

doFloodFill, doRTree, doNaive, doSafeZone :: [Coord] -> IO ()

doFloodFill cs = case fieldResult (floodFill cs) of
  Nothing -> putStrLn "Could not perform flood fill"
  Just r -> showResult r

doRTree cs = do
  let tree = RT.fromPoints (zip cs cs)
  case treeResult tree of
    Nothing -> putStrLn "Could not use RTree"
    Just r -> showResult r

doNaive cs = case naive cs of
  Nothing -> putStrLn "Naive didn't work"
  Just r -> showResult r

doSafeZone cs = do
  let safe = safeZone 10000 cs
  putStrLn "SAFE ZONE SIZE:"
  print safe

showResult :: Pt1Answer -> IO ()
showResult (c, n) = putStrLn $ mconcat [ cstr, ": ", show n ]
  where
    cstr = concat ["<", show (px c), ",", show (py c), ">"]

bigField :: [Coord] -> [Coord]
bigField = L.nubOrd . (>>= \c -> C.invert <$> [c, C.scale 2 c])

manhattan :: Coord -> Coord -> Int
manhattan = C.manhattan

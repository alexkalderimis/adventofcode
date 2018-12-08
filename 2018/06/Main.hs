{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}

import           Control.Applicative
import           Control.Monad
import           Control.Monad.ST as ST
import           Data.Array.ST (STArray, freeze, newArray, writeArray, readArray)
import qualified Data.Array as A
import qualified Data.Array.ST as STA
import           Data.Char (toUpper)
import           Data.Foldable
import           Data.IntPSQ as PSQ
import           Data.Maybe
import           Data.Ord
import qualified Data.Ix as Ix
import qualified Data.List as L
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import qualified Data.Time.Clock as Clock

-- This exercise has three solutions, playing with techniques and
-- looking for performance.
-- 
-- In order of performance we have:
--   * flood-fill, which iteratively fills the field with straight-line
--   neighbours. This makes use of localised safe mutable arrays for
--   efficiency
--   * rtree, which uses a spacial index to look for nearest neighbours
--   efficiently
--   * The naive approach for reference, which checks every point
--   for every input node, so O(nxm).
--
-- Current example output: 
-- --------------------
--  FLOOD FILL:
--  <170,240>: 3907
--  0.147134s
--  --------------------
--  RTree
--  <170,240>: 3907
--  0.186773s
--  --------------------
--  NAIVE
--  <170,240>: 3907
--  0.213381s
--  --------------------
--  SAFE ZONE SIZE
--  42036
--  0.033237s

-- strict unpacked tuples
data Coord
  = Coord { _x :: {-# UNPACK #-} !Int
          , _y :: {-# UNPACK #-} !Int
          }
          deriving (Ord, Eq)

instance Show Coord where
  show (Coord x y) = concat ["<", show x, ",", show y, ">"]

parseCoord :: String -> Coord
parseCoord s = case break (== ',') s of
  (x,',':y) -> Coord (read x) (read y)
  _         -> error $ "Could not parse " ++ s

instance Ix.Ix Coord where
  range ((Coord x0 y0), (Coord x1 y1)) = [Coord x y | (x,y) <- Ix.range ((x0,y0), (x1,y1))]
  index ((Coord x0 y0), (Coord x1 y1)) (Coord a b) = Ix.index ((x0,y0),(x1,y1)) (a,b)
  inRange ((Coord x0 y0), (Coord x1 y1)) (Coord a b) = Ix.inRange ((x0,y0),(x1,y1)) (a,b)

data BoundingBox
  = BB { topLeft  :: Coord
       , topRight :: Coord
       , btmLeft  :: Coord
       , btmRight :: Coord
       } deriving (Show)

boundingBox :: [Coord] -> Maybe BoundingBox
boundingBox cs = do
  (minX,maxX) <- minmax (_x <$> cs)
  (minY,maxY) <- minmax (_y <$> cs)
  return $ BB { topLeft = Coord minX minY
              , topRight = Coord maxX minY
              , btmLeft = Coord minX maxY
              , btmRight = Coord maxX maxY
              }
  where
    minmax = foldl' (\mp x -> fmap (uncurry $ cmp x) mp <|> Just (x,x)) Nothing
    cmp x small big = (min small x, max big x)
    
bounds :: BoundingBox -> (Coord, Coord)
bounds bb = (topLeft bb, btmRight bb)

unbounds :: (Coord, Coord) -> BoundingBox
unbounds ((Coord x0 y0), (Coord x1 y1))
  = BB { topLeft  = Coord x0 y0
       , topRight = Coord x1 y0
       , btmLeft  = Coord x0 y1
       , btmRight = Coord x1 y1
       }

data Ownership = Unclaimed
               | Disputed
               | Owned {-# UNPACK #-} !Coord
               deriving (Show)

owner :: (Applicative f, MonadPlus f) => Ownership -> f Coord
owner (Owned x) = pure x
owner _         = mzero

claimed :: Ownership -> Bool
claimed Unclaimed = False
claimed _         = True

-- Safe zone impl. Pt2 of the exercise
safeZone :: Int -> BoundingBox -> [Coord] -> S.Set Coord
safeZone upperBound bb cs
  = S.fromList
  . fmap fst
  . filter ((< upperBound) . snd)
  . fmap (\c -> (c, manhattanSum c))
  $ Ix.range (bounds bb)
    where
      manhattanSum c = sum $ fmap (manhattan c) cs

displaySafeZone :: M.Map Coord Char -> BoundingBox -> S.Set Coord -> [String]
displaySafeZone names bb safe = fmap row [_y (topLeft bb) - 1 .. _y (btmRight bb) + 1]
  where
    row y = do
     x <- [_x (topLeft bb) - 1 .. _x (btmRight bb) + 1]
     let c = Coord x y
     return $ case (M.lookup c names, S.member c safe) of
       (Just name, _) -> toUpper name
       (_, True)      -> '#'
       _              -> '.'

-- General reusable components of the different approaches

-- Given the nearest-neighbour search function, produces the result
generalResult :: (Coord -> [Coord]) -> BoundingBox -> Maybe (Coord, Int)
generalResult uniqClosest bb
  = listToMaybe
  . L.sortBy (comparing $ Down . snd)
  . M.toList
  . M.fromListWith (+)
  . fmap (,1)
  . filter notEscaped
  $ (Ix.range (bounds bb) >>= uniqClosest)
    where
      notEscaped = flip S.notMember escaped
      escaped = escapees uniqClosest bb

onEdge :: BoundingBox -> Coord -> Bool
onEdge bb (Coord x y) = x == _x (topLeft bb) ||
                        x == _x (topRight bb) ||
                        y == _y (topLeft bb) ||
                        y == _y (btmLeft bb)

escapees :: (Coord -> [Coord]) -> BoundingBox -> S.Set Coord
escapees uniqClosest bb = S.fromList $ do
  c <- Ix.range (bounds bb)
  guard (onEdge bb c)
  uniqClosest c

-- Flood fill approach:

type Field = A.Array Coord Ownership

drawField :: (M.Map Coord Char) -> Field -> [String]
drawField names field = fmap row rows
  where
    bs = A.bounds field
    rows = [_y (fst bs) .. _y (snd bs)]
    cols = [_x (fst bs) .. _x (snd bs)]
    row y = do
     x <- cols
     let c = Coord x y
     return $ case (M.lookup c names, field A.! c) of
       (Just name, _) -> toUpper name
       (_, Unclaimed) -> ' '
       (_, Disputed)  -> '.'
       (_, Owned owner) -> fromMaybe '?' (M.lookup owner names)

floodFill :: [Coord] -> Maybe Field
floodFill cs = do
  bb <- boundingBox cs
  return $ runST $ do
    field <- newArray (bounds bb) Unclaimed
    mapM_ (\c -> writeArray field c (Owned c)) cs
    go field (M.fromList [(c, S.singleton c) | c <- cs])
  where
    go field frontier = do
      frontier' <- step field frontier
      if all S.null (M.elems frontier')
         then freeze field
         else go field frontier'

-- because we move along manhattan distance, we only move in straight lines, and
-- we only consider 4 neighbours at each fill
neighbours :: Coord -> [Coord]
neighbours (Coord x y) = [                 Coord x (pred y)
                         ,Coord (pred x) y                 , Coord (succ x) y
                                           ,Coord x (succ y)
                         ]

fieldResult :: Field -> Maybe (Coord, Int)
fieldResult fld = generalResult uniqClosest (unbounds $ A.bounds fld)
    where 
      uniqClosest c = owner (fld A.! c)

countClaims :: Field -> M.Map Coord Int
countClaims = M.fromListWith (+) . (>>= claimCount) . A.elems
  where
      claimCount o = (,1) <$> owner o

-- the escapees from the field, i.e those on the edges
-- which have infinite fields
fieldEscapees :: Field -> S.Set Coord
fieldEscapees fld = escapees (owner . (fld A.!)) (unbounds $ A.bounds fld)

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
      writeClaims (owner, claims) = flip mapM_ claims $ \claim ->
        writeArray field claim (Owned owner)
      writeConflict c =
        writeArray field c Disputed
      -- a claim is uncontested if it is only claimed once
      contest wave =
        let newClaims = M.unionsWith (+) . fmap (M.fromSet (pure 1)) $ M.elems wave
            uncontested = S.filter ((1 ==) . (newClaims M.!))
         in (M.map uncontested wave, M.keysSet (M.filter (> 1) newClaims))
      -- the next wave is all the straight-line neighbours of this frontier
      -- that are further from the group origin than the frontier
      nextWave bs origin frontLine = do
        c <- S.toList frontLine
        n <- neighbours c
        guard (Ix.inRange bs n)
        guard (manhattan origin n > manhattan origin c)
        pure n
      onlyUnclaimed = filterM (fmap (not.claimed) . readArray field)
      advance bs capital frontLine =
        S.fromList <$> onlyUnclaimed (nextWave bs capital frontLine)


--- The naive implementation.
-- This is not really slower than the RTree solution, when O2 is enabled.
-- However without optimisation or in ghci, it is much slower.
--
-- It is however much shorter though.

naive :: BoundingBox -> [Coord] -> Maybe (Coord, Int)
naive bb cs = generalResult uniqClosest bb
    where
      uniqClosest c = case L.sortBy (comparing $ manhattan c) cs of
        (a:b:_) | manhattan a c == manhattan b c -> mzero
        (a:_)                                    -> pure a

--- an RTree based implementation
-- This is faster than the naive solution, but only just

data RTree a = Leaf Coord a
             | Tip
             | Region BoundingBox (RTree a) (RTree a)
             deriving (Show)

data Placement = TL | Above  | TR
               | L  | Inside | R
               | BL | Below  | BR
               deriving (Show)

-- index an association list of coordinates into an RTree
index :: [(Coord, a)] -> RTree a
index = go 0
  where
    divide xs = splitAt (length xs `div` 2) xs
    split n cs = let cmp = if n `mod` 2 == 0 then _x else _y
                  in divide $ L.sortBy (comparing (cmp . fst)) cs
    go n cs = case cs of
      [] -> Tip
      [(c,a)] -> Leaf c a
      cs | Just bb <- boundingBox (fst <$> cs) ->
        let (as,bs) = split n cs
            m = n + 1
         in Region bb (go m as) (go m bs)
      _ -> error "impossible"

treeResult :: BoundingBox -> RTree Coord -> Maybe (Coord, Int)
treeResult bb tree = generalResult uniqClosest bb
    where
      uniqClosest c = case take 2 $ nearest tree c of
        [(a,da),(b,db)] | da == db -> mzero
        ((a,_):_)                  -> pure a

displayRTree :: M.Map Coord Char -> BoundingBox -> RTree Coord -> [String]
displayRTree names bb tree = fmap row [_y (topLeft bb) .. _y (btmRight bb)]
  where
    row y = do
     x <- [_x (topLeft bb) .. _x (btmRight bb)]
     let c = Coord x y
     return $ case (M.lookup c names) of
       Just name -> toUpper name
       Nothing -> let [(a,da),(b,db)] = take 2 (nearest tree c)
                   in if da == db
                         then '.'
                         else fromMaybe '?' (M.lookup a names)


-- nearest neighour search in the RTree
-- Returns the value and its distance
nearest :: Show a => RTree a -> Coord -> [(a, Int)]
nearest t c = go 1 (insertTree 0 t PSQ.empty)
  where
    insertTree n t = case priorityDistance c t of
      Nothing -> id
      Just p  -> PSQ.insert n p t
    go n q = case PSQ.minView q of
               Nothing -> []
               Just (_, p, t, q') -> case t of
                 Tip          -> go (n + 1) q'
                 Leaf _ a     -> (a,p) : go (n + 1) q'
                 Region _ l r -> go (n + 2) (insertTree n l $ insertTree (n + 1) r $ q')

placement :: BoundingBox -> Coord -> Placement
placement bb c@(Coord x y)
  | Ix.inRange (bounds bb) c                   = Inside
  | x < _x (topLeft bb) = if | y < _y (topLeft bb) -> TL
                             | y > _y (btmLeft bb) -> BL
                             | otherwise           -> L
  | x > _x (topRight bb) = if | y < _y (topRight bb) -> TR
                              | y > _y (btmRight bb) -> BR
                              | otherwise            -> R
  | y < _y (topLeft bb) = Above
  | y > _y (btmLeft bb) = Below
  | otherwise = error "impossible"

priorityDistance :: Coord -> RTree a -> Maybe Int
priorityDistance _ Tip = Nothing
priorityDistance c (Leaf loc _) = Just $ manhattan c loc
priorityDistance c (Region bb@BB{..} _ _) = return . manhattan c $ case placement bb c of
  TL     -> topLeft
  Above  -> Coord (_x c) (_y topLeft)
  TR     -> topRight
  L      -> Coord (_x topLeft) (_y c)
  Inside -> c
  R      -> Coord (_x topRight) (_y c)
  BL     -> btmLeft
  Below  -> Coord (_x c) (_y btmLeft)
  BR     -> btmRight

-- the manhattan distance calculation
manhattan :: Coord -> Coord -> Int
manhattan a b = straightedge _x a b + straightedge _y a b
  where
    straightedge f p0 p1 = abs (f p0 - f p1)

examplePoints :: [Coord]
examplePoints = fmap (uncurry Coord)
  [(1, 1)
  ,(1, 6)
  ,(8, 3)
  ,(3, 4)
  ,(5, 5)
  ,(8, 9)
  ]

main :: IO ()
main = getContents >>= run

example :: IO ()
example = do
  let Just bb = boundingBox examplePoints
      Just fld = floodFill examplePoints
      names = M.fromList (zip examplePoints ['a' .. ])
      tree = index (zip examplePoints examplePoints)
  putStrLn "flood-fill"
  draw (drawField names fld)
  traverse_ print (fieldResult fld)
  putStrLn "SIZES"
  print (countClaims fld)
  putStrLn "ESCAPEES"
  print (fieldEscapees fld)

  putStrLn (replicate 20 '-')
  putStrLn "rtree"
  draw (displayRTree names bb tree)
  traverse_ print (treeResult bb tree)
    where
      draw = mapM_ (putStrLn . ('|' : ))

run :: String -> IO ()
run inputData = do
  let coords = fmap parseCoord . lines $ inputData
      tree = index (zip coords coords)
  case boundingBox coords of
    Nothing -> error "Could not establish bounding box"
    Just bb -> do time $ doFloodFill coords
                  time $ doRTree bb tree
                  time $ doNaive bb coords
                  time $ doSafeZone bb coords
  where
    hline title = putStrLn (replicate 20 '-') >> putStrLn title
    time act = do
      start <- Clock.getCurrentTime
      act
      end <- Clock.getCurrentTime
      print (Clock.diffUTCTime end start)
    doSafeZone bb cs = do
      let safe = safeZone 10000 bb cs
      hline "SAFE ZONE SIZE"
      putStrLn $ show (S.size safe)
    doNaive bb cs = do
      hline "NAIVE"
      case naive bb cs of
        Nothing -> putStrLn "Naive didn't work"
        Just (c,n) -> do putStrLn $ show c ++ ": " ++ show n
    doFloodFill cs = do
      hline "FLOOD FILL: "
      case floodFill cs >>= fieldResult of
        Nothing -> putStrLn "Could not perform flood fill"
        Just (coord, s) -> putStrLn $ show coord ++ ": " ++ show s
    doRTree bb tree = do
      hline "RTree"
      case treeResult bb tree of
        Nothing -> putStrLn "Could use RTree"
        Just (c,n) -> do
          putStrLn $ show c ++ ": " ++ show n

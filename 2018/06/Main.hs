{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE MultiWayIf #-}

import qualified Debug.Trace as Debug
import Data.IntPSQ as PSQ
import Data.Semigroup
import           Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Data.Maybe
import Control.Monad
import Data.Foldable
import Data.Ord
import qualified Data.Ix as Ix
import qualified Data.List as L
import Data.Char (toUpper)
import Control.Applicative

data Coord
  = Coord { _x :: {-# UNPACK #-} !Int
          , _y :: {-# UNPACK #-} !Int
          }
          deriving (Show, Ord, Eq)

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

type Empire = S.Set Coord

data FillState
  = FillState { empires :: M.Map Coord Empire
              , frontier :: M.Map Coord Empire
              , escaped :: S.Set Coord
              , claimed :: S.Set Coord
              , contested :: S.Set Coord
              , iterations :: Int
              } deriving Show

floodFill :: [Coord] -> Maybe FillState
floodFill cs = do
  bb <- boundingBox cs
  return $ go bb (initFillState cs)
  where
    go bb fs = let fs' = step bb fs
                in if all S.null (M.elems $ frontier fs')
                      then fs'
                      else go bb fs'

-- because we move along manhattan distance, we only move in straight lines, and
-- we only consider 4 neighbours at each fill
neighbours :: Coord -> [Coord]
neighbours (Coord x y) = [                 Coord x (pred y)
                         ,Coord (pred x) y                 , Coord (succ x) y
                                           ,Coord x (succ y)
                         ]

initFillState :: [Coord] -> FillState
initFillState cs =
  let empires = M.fromList [(c, S.singleton c) | c <- L.nub cs]
   in FillState { empires = empires
                , frontier = empires
                , escaped = mempty
                , claimed = S.fromList cs
                , contested = mempty
                , iterations = 0
                }

type Owners = M.Map Coord Coord

owners :: FillState -> Owners
owners fs = M.unions [M.fromSet (pure capital) empire | (capital, empire) <- M.toList (empires fs)]

currentBiggestFiniteEmpire :: FillState -> Maybe (Coord, S.Set Coord)
currentBiggestFiniteEmpire fs = listToMaybe
                              . dropWhile (\(c,_) -> S.member c (escaped fs))
                              . L.sortBy (comparing (Down . S.size . snd))
                              . M.toList
                              $ empires fs

step :: BoundingBox -> FillState -> FillState
step bb fs =
  let wave = M.mapWithKey advance (frontier fs)
      (uncontested, conflicts) = contest wave
      claimed' = foldl' S.union (claimed fs) (M.elems wave)
   in fs { empires = M.unionWith S.union uncontested (empires fs)
         , frontier = M.map (S.filter (not . outOfBounds)) wave
         , escaped = S.union (escapees uncontested) (escaped fs)
         , claimed = claimed'
         , contested = S.union (contested fs) conflicts
         , iterations = 1 + iterations fs
         }
  where advance capital fnt = S.fromList $ do
          c <- S.toList fnt
          n <- neighbours c
          guard (manhattan n capital > manhattan c capital)
          guard (S.notMember n (claimed fs))
          guard (S.notMember n (contested fs))
          return n
        outOfBounds (Coord x y) = or [x < _x (topLeft bb)
                                     ,x > _x (topRight bb)
                                     ,y < _y (topLeft bb)
                                     ,y > _y (btmLeft bb)
                                     ]
        escapees wave = S.fromList [ c | (c, cs) <- M.toList wave, any outOfBounds cs ] 
        contest wave = let newClaims = M.unionsWith (+) . fmap (M.fromSet (pure 1)) $ M.elems wave
                           uncontested = M.map (S.filter ((1 ==) . (newClaims M.!))) wave
                        in (uncontested, M.keysSet (M.filter (> 1) newClaims))

--- an RTree based implementation

data RTree a = Leaf Coord a
             | Tip
             | Region BoundingBox (RTree a) (RTree a)
             deriving (Show)

data Placement = TL | Above  | TR
               | L  | Inside | R
               | BL | Below  | BR
               deriving (Show)

biggestFiniteEmpire :: BoundingBox -> RTree Coord -> Maybe (Coord, Int)
biggestFiniteEmpire bb tree
  = listToMaybe
  $ L.sortBy (comparing (Down . snd))
  $ M.toList
  $ M.fromListWith (+)
  $ fmap (\c -> (c,1))
  $ filter (`S.notMember` infiniteEmpires)
  $ (>>= (maybeToList . uniqClosest))
  $ Ix.range (bounds bb)
    where
      onEdge (Coord x y) = x == _x (topLeft bb) ||
                           x == _x (topRight bb) ||
                           y == _y (topLeft bb) ||
                           y == _y (btmLeft bb)
      uniqClosest c = case take 2 $ nearest tree c of
        [a,b] | manhattan a c == manhattan b c -> Nothing
        (a:_) -> Just a
      infiniteEmpires = S.fromList . catMaybes $ do
        c <- Ix.range (bounds bb)
        guard (onEdge c)
        return (uniqClosest c)

displayRTree :: M.Map Coord Char -> BoundingBox -> RTree Coord -> [String]
displayRTree names bb tree = fmap row [_y (topLeft bb) - 1 .. _y (btmRight bb) + 1]
  where
    row y = do
     x <- [_x (topLeft bb) - 1 .. _x (btmRight bb) + 1]
     let c = Coord x y
     return $ case (M.lookup c names) of
       Just name -> toUpper name
       Nothing -> let [a,b] = take 2 (nearest tree c)
                   in if manhattan a c == manhattan b c
                         then '.'
                         else fromMaybe '?' (M.lookup a names)

nearest :: Show a => RTree a -> Coord -> [a]
nearest t c = go 1 (insertTree 0 t PSQ.empty)
  where
    insertTree n t = case priorityDistance c t of
      Nothing -> id
      Just p  -> PSQ.insert n p t
    go n q = case PSQ.minView q of
               Nothing -> []
               Just (_, _, t, q') -> case t of
                 Tip      -> []
                 Leaf _ a -> a : go (n + 1) q'
                 Region _ l r -> go (n + 2) (insertTree n l $ insertTree (n + 1) r $ q')

bounds :: BoundingBox -> (Coord, Coord)
bounds bb = (topLeft bb, btmRight bb)

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

drawFillState :: (M.Map Coord Char) -> BoundingBox -> Owners -> [String]
drawFillState names bb owners = fmap row [_y (topLeft bb) - 1 .. _y (btmRight bb) + 1]
  where
    row y = do
     x <- [_x (topLeft bb) - 1 .. _x (btmRight bb) + 1]
     let c = Coord x y
     return $ case (M.lookup c names, M.lookup c owners) of
       (Just name, _) -> toUpper name
       (_, Just p)    -> fromMaybe '?' (M.lookup p names)
       _              -> '.'

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
    
main :: IO ()
main = do
  getContents >>= run

run :: String -> IO ()
run inputData = do
  let coords = fmap parseCoord . lines $ inputData
  case boundingBox coords of
    Nothing -> error "Could not establish bounding box"
    Just bb -> do doFloodFill coords
                  doRTree bb coords
  where
    doFloodFill cs = do
      case floodFill cs >>= currentBiggestFiniteEmpire of
        Nothing -> putStrLn "Could not perform flood fill"
        Just (coord, s) -> do
          putStrLn "FLOOD FILL: "
          putStrLn $ show coord ++ ": " ++ show (S.size s)
    doRTree bb cs = do
      let tree = index (zip cs cs)
      case biggestFiniteEmpire bb tree of
        Nothing -> putStrLn "Could use RTree"
        Just (c,n) -> do
          putStrLn "RTree"
          putStrLn $ show c ++ ": " ++ show n

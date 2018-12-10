{-# LANGUAGE BangPatterns #-}

import Control.Monad
import Data.Semigroup
import Data.Foldable
import System.Environment (withArgs, getArgs)
import qualified Data.List as L
import qualified Data.Set as S
import qualified Data.Time.Clock as Clock

import qualified Data.List.NonEmpty as NE
import           Data.List.NonEmpty (NonEmpty)

import Test.Hspec

data Point = Point { px :: !Int, py :: !Int }
  deriving (Show, Eq, Ord)

data Velocity = Velocity { dx :: !Int, dy :: !Int }
  deriving (Show, Eq)

-- tested this with NonEmpty and Vector. NE was nicer, and faster in places.
type Coll = NonEmpty
type Sky = Coll Point

data Bounds = Bounds
  { boundsOrigin :: !Point
  , boundsWidth :: !Int
  , boundsHeight :: !Int
  } deriving (Show, Eq)

instance Semigroup Bounds where
  b0 <> b1 = let minx = min (x b0) (x b1)
                 miny = min (y b0) (y b1)
                 maxx = max (x b0 + w b0) (x b1 + w b1)
                 maxy = max (y b0 + h b0) (y b1 + h b1)
              in Bounds { boundsOrigin = Point minx miny
                        , boundsWidth = maxx - minx
                        , boundsHeight = maxy - miny
                        }
    where x = px . boundsOrigin
          y = py . boundsOrigin
          w = boundsWidth
          h = boundsHeight

-- Print message and number of steps taken
main :: IO ()
main = do
  args <- getArgs
  case args of
    ["test"] -> withArgs [] (hspec spec)
    ["run"]  -> do strs <- lines <$> getContents
                   time $ case getMessage strs of
                            Just (i, sky) -> do printSky sky
                                                putStrLn $ show i ++ " steps"
                            Nothing -> putStrLn "NO MESSAGE"
    _        -> putStrLn "missing argument: test, run"
  where
    time act = do
      start <- Clock.getCurrentTime
      act
      end <- Clock.getCurrentTime
      print (Clock.diffUTCTime end start)

-------------------

area :: Bounds -> Int
area bs = boundsHeight bs * boundsWidth bs

-- skies are never empty
bounds :: Sky -> Bounds
bounds = foldl1 (<>) . fmap bound
  where bound p = Bounds p 1 1

parsePoint :: String -> (Point, Velocity)
parsePoint s = 
  let s'        = drop posLen s
      (pos,s'') = break (== 'v') s'
      vel      = drop velLen s''
      (px, py) = parsePair pos
      (dx, dy) = parsePair vel
   in (Point px py, Velocity dx dy)
  where
    posLen = length ("position=" :: String)
    velLen = length ("velocity=" :: String)

parsePair :: String -> (Int, Int)
parsePair ('<' : s) =
  let (xstr, (',':s')) = break (== ',') s
      (ystr, _) = break (== '>') s'
   in (read xstr, read ystr)
parsePair s = error ("parsePair: " ++ s)

tick :: Coll Velocity -> Sky -> Sky
tick = NE.zipWith move
  where move (Velocity dx dy) (Point px py) = Point (px + dx)
                                                    (py + dy)

getMessage :: [String] -> Maybe (Int, Sky)
getMessage strs = do
  pvs <- fmap parsePoint <$> NE.nonEmpty strs
  let sky = localMinimum . fmap withBounds $ zip [1 ..] (skies pvs)
  return sky
  where
    withBounds (i, sky) = (i, sky, bounds sky)
    localMinimum ss = case take 2 ss of
      [(i,s0,b0), (_,_,b1)] | area b0 < area b1 -> (i, s0)
      _  -> localMinimum (drop 1 ss)

-- for interactively looking at a sequence of skies
play :: Int -> [String] -> IO ()
play n = mapM_ printFrame . take n . skies . fmap parsePoint . NE.fromList
  where
    printFrame sky = do
      putStrLn $ "AREA: " ++ show (area $ bounds sky)
      printSky sky

-- infinite list of skies
skies :: Coll (Point, Velocity) -> [Sky]
skies pvs = L.unfoldr (k (tick vs)) s0
  where
    (s0, vs) = NE.unzip pvs
    k f a = let b = f a in Just (b,b)

drawSky :: Sky -> [String]
drawSky sky = row <$> take (boundsHeight bs) [0 ..]
  where
    row  y   = cell y <$> take (boundsWidth bs) [0 ..]
    cell y x = if S.member (Point x y) stars then '#' else '.' 
    bs = bounds sky
    origin = boundsOrigin bs
    -- translate points to a (0,0) origin sky
    translate (Point x y) = Point (x - px origin) (y - py origin)
    stars = foldr S.insert mempty (fmap translate sky)

printSky :: Sky -> IO ()
printSky = mapM_ putStrLn . drawSky

bigPoints :: [String]
bigPoints = 
  ["position=<-54530, -54537> velocity=< 5,  5>"
  ,"position=< 22070, -54534> velocity=<-2,  5>"
  ,"position=<-54581, -32648> velocity=< 5,  3>"
  ,"position=< 22060, -54537> velocity=<-2,  5>"
  ]

examplePoints :: [String]
examplePoints = 
  ["position=< 9,  1> velocity=< 0,  2>"
  ,"position=< 7,  0> velocity=<-1,  0>"
  ,"position=< 3, -2> velocity=<-1,  1>"
  ,"position=< 6, 10> velocity=<-2, -1>"
  ,"position=< 2, -4> velocity=< 2,  2>"
  ,"position=<-6, 10> velocity=< 2, -2>"
  ,"position=< 1,  8> velocity=< 1, -1>"
  ,"position=< 1,  7> velocity=< 1,  0>"
  ,"position=<-3, 11> velocity=< 1, -2>"
  ,"position=< 7,  6> velocity=<-1, -1>"
  ,"position=<-2,  3> velocity=< 1,  0>"
  ,"position=<-4,  3> velocity=< 2,  0>"
  ,"position=<10, -3> velocity=<-1,  1>"
  ,"position=< 5, 11> velocity=< 1, -2>"
  ,"position=< 4,  7> velocity=< 0, -1>"
  ,"position=< 8, -2> velocity=< 0,  1>"
  ,"position=<15,  0> velocity=<-2,  0>"
  ,"position=< 1,  6> velocity=< 1,  0>"
  ,"position=< 8,  9> velocity=< 0, -1>"
  ,"position=< 3,  3> velocity=<-1,  1>"
  ,"position=< 0,  5> velocity=< 0, -1>"
  ,"position=<-2,  2> velocity=< 2,  0>"
  ,"position=< 5, -2> velocity=< 1,  2>"
  ,"position=< 1,  4> velocity=< 2,  1>"
  ,"position=<-2,  7> velocity=< 2, -2>"
  ,"position=< 3,  6> velocity=<-1, -1>"
  ,"position=< 5,  0> velocity=< 1,  0>"
  ,"position=<-6,  0> velocity=< 2,  0>"
  ,"position=< 5,  9> velocity=< 1, -2>"
  ,"position=<14,  7> velocity=<-2,  0>"
  ,"position=<-3,  6> velocity=< 2, -1>"
  ]

spec :: Spec
spec = do
  describe "getMessage" $ do
    let msg = [ "#...#..###"
              , "#...#...#."
              , "#...#...#."
              , "#####...#."
              , "#...#...#."
              , "#...#...#."
              , "#...#...#."
              , "#...#..###"
              ]
        ans = getMessage examplePoints
    it "should descern the message" $ do
      fmap (drawSky.snd) ans `shouldBe` Just msg
    it "should know how long it took" $ do
      fmap fst ans `shouldBe` Just 3
  describe "drawSky" $ do
    let diamondSky =
          ["position=< 0,  0> velocity=< 0,  2>"
          ,"position=< 1,  1> velocity=< 0,  2>"
          ,"position=< 0, -5> velocity=< 0,  2>"
          ,"position=< 5,  0> velocity=<-1,  0>"
          ,"position=< 0,  5> velocity=<-1,  1>"
          ,"position=<-5,  0> velocity=<-2, -1>"
          ]
    it "should draw the sky correctly" $ do
      let sky = NE.fromList . fmap (fst . parsePoint)
                           $ diamondSky
          expected = [ ".....#....."
                     , "..........."
                     , "..........."
                     , "..........."
                     , "..........."
                     , "#....#....#"
                     , "......#...."
                     , "..........."
                     , "..........."
                     , "..........."
                     , ".....#....."
                     ]
      drawSky sky `shouldBe` expected
  describe "parsePoint" $ do
    it "should parse bigPoints correctly" $ do
      let expected = [(Point {px = -54530, py = -54537}
                      ,Velocity {dx = 5, dy = 5}
                      )
                     ,(Point {px = 22070, py = -54534}
                      ,Velocity {dx = -2, dy = 5}
                      )
                     ,(Point {px = -54581, py = -32648}
                      ,Velocity {dx = 5, dy = 3}
                      )
                     ,(Point {px = 22060, py = -54537}
                      ,Velocity {dx = -2, dy = 5}
                      )
                     ]
      fmap parsePoint bigPoints `shouldBe` expected
  describe "bounds" $ do
    it "should get correct bounds from a small sky" $ do
      let sky = NE.fromList [Point 0 0
                           ,Point 5 5
                           ,Point 20 10
                           ]
      bounds sky `shouldBe` (Bounds (Point 0 0) 21 11)


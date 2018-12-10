import Test.Hspec
import System.Environment (withArgs, getArgs)
import qualified Data.Time.Clock as Clock
import qualified Data.List as L
import Data.Semigroup
import Data.Foldable
import qualified Data.Set as S
import qualified Data.List.NonEmpty as NE
import           Data.List.NonEmpty (NonEmpty)

data Point = Point { px :: Int, py :: Int }
  deriving (Show, Eq, Ord)

data Velocity = Velocity { dx :: Int, dy :: Int }
  deriving (Show, Eq)

type Sky = NonEmpty Point

data Bounds = Bounds
  { boundsOrigin :: Point
  , boundsWidth :: Int
  , boundsHeight :: Int
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

tick :: NonEmpty Velocity -> Sky -> Sky
tick vels = NE.zipWith move vels
  where move (Velocity dx dy) (Point px py) = Point (px + dx)
                                                    (py + dy)

getMessage :: [String] -> Maybe Sky
getMessage strs = do
  pvs <- NE.nonEmpty $ fmap parsePoint strs
  let s0  = fmap fst pvs
      sky = localMinimum (fmap withBounds $ NE.toList $ skies pvs)
  return sky
  where
    withBounds sky = (sky, bounds sky)
    localMinimum ss = case take 2 ss of
      [(s0,b0), (s1,b1)] | area b0 < area b1 -> s0
      _  -> localMinimum (drop 1 ss)

area :: Bounds -> Int
area bs = boundsHeight bs * boundsWidth bs

play :: Int -> [String] -> IO ()
play n strs = do
  let pvs = parsePoint <$> NE.fromList strs
  mapM_ printFrame (NE.take n $ skies pvs)
  where
    printFrame sky = do
      putStrLn $ "AREA: " ++ show (area $ bounds sky)
      printSky sky

skies :: NonEmpty (Point, Velocity) -> NonEmpty Sky
skies pvs = NE.unfoldr (k (tick vs)) s0
  where
    (s0, vs) = NE.unzip pvs
    k f a = let b = f a in (b, Just b)

drawSky :: Sky -> [String]
drawSky sky = fmap row $ take (boundsHeight bs) [0 ..]
  where
    bs = bounds sky
    -- translate points to a (0,0) origin sky
    translate (Point x y) = let o = boundsOrigin bs
                             in Point (x - px o) (y - py o)
    stars = foldr S.insert mempty (fmap translate sky)
    row i = fmap (cell i) $ take (boundsWidth bs) [0 ..]
    cell i j = if S.member (Point j i) stars then '#' else '.' 

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

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["test"] -> withArgs [] (hspec spec)
    ["pt1"]  -> do strs <- lines <$> getContents
                   time $ case getMessage strs of
                            Just sky -> printSky sky
                            Nothing -> putStrLn "NO MESSAGE"
    ["pt2"]  -> error "not implemented"
    _        -> putStrLn "missing argument: test, pt1, pt2"
  where
    time act = do
      start <- Clock.getCurrentTime
      act
      end <- Clock.getCurrentTime
      print (Clock.diffUTCTime end start)

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
    it "should descern the message" $ do
      fmap drawSky (getMessage examplePoints) `shouldBe` Just msg
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


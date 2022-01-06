{-# LANGUAGE OverloadedStrings #-}

import           Control.Arrow
import           Control.Lens
import           Control.Monad
import           Data.Attoparsec.Text    (decimal, letter, signed)
import qualified Data.List.Extra         as L
import qualified Data.IntMap.Strict      as M
import           Data.Maybe
import           Data.Ord
import qualified Data.Text               as Text
import           Text.Parser.Char        (newline)
import           Text.Parser.Combinators (sepBy1)

import           Elves
import           Elves.Advent
import           Elves.Coord
import           Elves.RTree             (RTree)
import qualified Elves.RTree             as RT

type Point = (Int,Int,Int)
type ParticleID = Int

data Particle = Particle
  { pid :: !ParticleID
  , pos :: !Point
  , vol :: !Point
  , acc :: !Point
  } deriving (Show, Eq)

main :: IO ()
main = day 20 parser pt1 pt2 test
  where
    -- the particle that stays closest is the one with the minimum acceleration
    pt1 sys = print $ L.minimumBy (comparing (manhattan origin . acc)) sys

-- no more collisions are possible when no particles are going to invert
-- and all particles are getting further away from each other
pt2 = runTilInversion 0
  where
    runTilInversion n sys = do
      -- putStrLn $ "pt2-a: " ++ show ( M.size sys)
      let sys' = tickColliding sys
       in if any willInvert sys'
          then runTilInversion (n + 1) sys'
          else putStrLn ("Starting runWhileApproaching after " <> show n <> " ticks")
               >> runWhileApproaching 0 sys' (M.fromAscList [(pid p, []) | p <- sys'])
    runWhileApproaching n sys history = do
      -- putStrLn $ "pt2-b: " ++ show (M.size sys)
      let sys'   = tickColliding sys
          t      = RT.fromPoints $ zip (pos <$> sys') (repeat ())
          d      = fromJust . distanceToNearest t
          dists' = M.fromAscList [ (pid p, d (pos p) : history M.! pid p) | p <- sys' ]
      if length sys' < 2 || all receding dists'
         then do putStrLn ("All receding after " <> show n <> " ticks")
                 print (length sys')
         else runWhileApproaching (n + 1) sys' dists'
    receding (a:b:_) = a >= b
    receding _       = False

distanceToNearest :: RTree Point () -> Point -> Maybe Double
distanceToNearest t p = measure p . foundPoint <$> RT.nearestNeighbour measure p t
  where
    foundPoint :: (Bounds Point, ()) -> Point
    foundPoint = fst . fst
    measure :: Point -> Point -> Double
    measure = straightLine

test = do
  describe "example system" $ do
    let msys = parseOnly parser exampleSystem
    it "parsed correctly" $ do
      msys `shouldBe` Right [Particle 0 (3,0,0) (2,0,0) (-1,0,0)
                            ,Particle 1 (4,0,0) (0,0,0) (-2,0,0)
                            ]
    it "ticks correctly" $ do
      let (Right sys) = msys
      (fmap pos . (!! 3) . iterate (fmap tick) $ sys) `shouldBe` [(3,0,0),(-8,0,0)]
    describe "will invert" $ do
      it "knows which particles are going to invert" $ do
        let (Right sys) = msys
        fmap willInvert sys `shouldBe` [True,False]

parser = fmap identify (particle `sepBy1` newline)
  where
    identify = zipWith (\i p -> p { pid = i }) [0..]
    particle = Particle 0 <$> ("p=" *> point <* ", ")
                          <*> ("v=" *> point <* ", ")
                          <*> ("a=" *> point)
    int = signed decimal
    point = (,,) <$> ("<" *> int)
                 <*> ("," *> int)
                 <*> ("," *> int <* ">")

exampleSystem = Text.unlines
  ["p=<3,0,0>, v=<2,0,0>, a=<-1,0,0>"
  ,"p=<4,0,0>, v=<0,0,0>, a=<-2,0,0>"
  ]

exampleCollisions = Text.unlines
  ["p=<-6,0,0>, v=<3,0,0>, a=<0,0,0>"
  ,"p=<-4,0,0>, v=<2,0,0>, a=<0,0,0>"
  ,"p=<-2,0,0>, v=<1,0,0>, a=<0,0,0>"
  ,"p=<3,0,0>, v=<-1,0,0>, a=<0,0,0>"
  ]

tickColliding :: [Particle] -> [Particle]
tickColliding = L.sortOn pid . removeCollisions . collide . fmap tick
  where
    removeCollisions = catMaybes . fmap unscathed
    collide = L.groupOn pos . L.sortOn pos
    unscathed [x] = Just x
    unscathed _ = Nothing

tick :: Particle -> Particle
tick (Particle pid p v a) = Particle pid (translate v' p) v' a
  where v' = translate a v

-- In a single dimension: a particle will invert if:
-- - it is not motionless
-- - it has non-zero acceleration
-- - it is accelerating in the opposite direction of its current motion
--
-- i.e, like a ball chucked in the air, it is still going up, and has yet to come down.
--
-- In N dimensions, this applies to all components.
willInvert :: Particle -> Bool
willInvert (Particle _ _ v a) = any inverting dimensions
  where
    inverting (Lens d) = (v ^. d) /= 0 && (a ^. d) /= 0 && (a ^. d < 0) /= (v ^. d < 0)

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TypeFamilies #-}

module Main (main) where

import           Control.Lens
import           Data.Attoparsec.Text    (decimal, letter, signed)
import qualified Data.List.Extra         as L
import qualified Data.IntMap.Strict      as M
import           Data.Ord
import qualified Data.Text               as Text
import           Text.Parser.Char        (newline)
import           Text.Parser.Combinators (sepBy1)

import           Elves
import           Elves.Advent            (day)
import           Elves.Coord
import           Elves.Coord.Strict (Point3(..), px, py, pz)
import           Elves.RTree             (RTree)
import qualified Elves.RTree             as RT

type ParticleID = Int
type Distances = M.IntMap [Double]
type Neighbours = RTree Point3 ()

data Particle = Particle
  { pid :: !ParticleID
  , pos :: !Point3
  , vol :: !Point3
  , acc :: !Point3
  } deriving (Show, Eq)

main :: IO ()
main = day 20 parser pt1 pt2 test
  where
    -- the particle that stays closest is the one with the minimum acceleration
    pt1 sys = print $ L.minimumBy (comparing (manhattan origin . acc)) sys

-- no more collisions are possible when no particles are going to invert
-- and all particles are getting further away from each other
pt2 sys = do
  let sys'      = runTilInversion sys
      distances = initialDistances sys'

  print . length $ runWhileApproaching sys' distances

  where
    runTilInversion !sys =
      let sys' = tickColliding sys
       in if any willInvert sys'
          then runTilInversion sys'
          else sys' -- <$ putStrLn (show n <> " ticks")

    runWhileApproaching !sys !history =
      let sys'   = tickColliding sys
          d      = fromJust . distanceToNearest (neighbours sys')
          dists' = updateDistances d sys' history
       in if length sys' < 2 || all receding dists'
          then sys' -- <$ putStrLn (show n <> " ticks")
          else runWhileApproaching sys' dists'
         
    receding (a : b : _) = a >= b
    receding _ = False

{-# INLINE initialDistances #-}
initialDistances :: [Particle] -> Distances
initialDistances sys = M.fromAscList [ (pid p, []) | p <- sys ]

{-# INLINE updateDistances #-}
updateDistances :: (Point3 -> Double) -> [Particle] -> Distances -> Distances
updateDistances d sys history = M.fromAscList $ do
  p <- sys
  let curr = d (pos p)
      prev = history M.! pid p
      now = take 2 (curr : prev)
  now `seq` pure (pid p, now)

{-# INLINE neighbours #-}
neighbours :: [Particle] -> Neighbours
neighbours sys = RT.fromPoints $ zip (pos <$> sys) (repeat ())

{-# INLINE distanceToNearest #-}
distanceToNearest :: Neighbours -> Point3 -> Maybe Double
distanceToNearest t p = measure p . foundPoint <$> RT.nearestNeighbour straightLine p t
  where
    foundPoint :: (Bounds Point3, ()) -> Point3
    foundPoint = fst . fst
    measure = straightLine

test = do
  describe "example system" $ do
    let esys = parseOnly parser exampleSystem
    it "parsed correctly" $ do
      esys `shouldBe` Right [ Particle 0 (P3 3 0 0) (P3 2 0 0) (P3 (-1) 0 0)
                            , Particle 1 (P3 4 0 0) (P3 0 0 0) (P3 (-2) 0 0)
                            ]
    context "given successful parsing" $ do
      let (Right sys) = esys
      it "ticks correctly" $ do
        (fmap pos . (!! 3) $ iterate tickColliding sys) `shouldBe` [P3 3 0 0, P3 (-8) 0 0]
      it "knows which particles are going to invert" $ do
        fmap willInvert sys `shouldBe` [True,False]
  describe "tickColliding" $ do
    it "removes collisions" $ do
      let sys = [ Particle 0 (P3 3 0 0) (P3 0 0 0) (P3 0 0 0)
                , Particle 1 (P3 4 0 0) (P3 0 0 0) (P3 (-2) 0 0)
                , Particle 2 (P3 1 0 0) (P3 0 0 0) (P3 1 0 0)
                ]
      tickColliding sys `shouldBe` take 1 sys

parser = identify <$> particle `sepBy1` newline
  where
    identify = zipWith (\i p -> p { pid = i }) [0..]
    particle = Particle 0 <$> ("p=" *> point <* ", ")
                          <*> ("v=" *> point <* ", ")
                          <*> ("a=" *> point)
    int = signed decimal
    point = P3 <$> ("<" *> int)
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
tickColliding = L.sortOn pid . collide . fmap tick
  where
    collide = L.concatMap unscathed . L.groupOn pos . L.sortOn pos
    unscathed [x] = [x]
    unscathed _   = []

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
willInvert (Particle _ _ v a) = any inverting [px, py, pz]
  where
    inverting f = f v /= 0 && f a /= 0 && ((f a < 0) /= (f v < 0))

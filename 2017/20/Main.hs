{-# LANGUAGE OverloadedStrings #-}

import           Data.Attoparsec.Text    (letter, signed, decimal)
import qualified Data.List               as L
import           Data.Maybe
import qualified Data.Text               as Text
import           Text.Parser.Char        (newline)
import           Text.Parser.Combinators (sepBy1)
import Control.Lens
import Control.Monad
import Data.Ord
import qualified Data.Map.Strict as M
import           Data.Map.Strict (Map)
import qualified Data.Map.Merge.Strict as M

import Elves
import Elves.Advent
import Elves.Coord
import Elves.RTree (RTree)
import qualified Elves.RTree as RT

type Point = (Int,Int,Int)

data Particle = Particle { pos :: Point, vol :: Point, acc :: Point }
  deriving (Show, Eq)

main :: IO ()
main = day 20 (zip [0..] <$> parser) pt1 pt2 test
  where
    -- the particle that stays closest is the one with the minimum acceleration
    pt1 sys = print $ L.minimumBy (comparing (manhattan (0,0,0) . acc . snd)) sys

-- no more collisions are possible when no particles are going to invert
-- and all particles are getting further away from each other
pt2 sys = pt2A $ M.fromList [(pos pnt, (pid, pnt)) | (pid, pnt) <- sys]
  where
    pt2A sys =
      let sys' = tickColliding sys
       in if any (willInvert . snd) sys'
          then pt2A sys'
          else pt2B sys' (M.fromList [(pid, []) | pid <- fst <$> M.elems sys'])
    pt2B sys dists = do
      let sys' = tickColliding sys
          t = RT.index [(pos, ()) | pos <- M.keys sys']
          d  pos = maybeToList . fmap (measure pos . fst) $ RT.nearestNeighbour measure pos t
          ds pid = join . maybeToList $ M.lookup pid dists
          dists' = M.fromList [(pid, d (pos pnt) ++ ds pid) | (pid,pnt) <- M.elems sys']
      if M.size dists' < 2 || all receding dists'
         then print (M.size sys')
         else pt2B sys' dists'
    receding (a:b:_) = a >= b
    receding _ = False
    measure :: Point -> Point -> Double
    measure = straightLine

test = do
  describe "example system" $ do
    let msys = parseOnly parser exampleSystem
    it "parsed correctly" $ do
      msys `shouldBe` Right [Particle (3,0,0) (2,0,0) (-1,0,0)
                            ,Particle (4,0,0) (0,0,0) (-2,0,0)
                            ]
    it "ticks correctly" $ do
      let (Right sys) = msys
      (fmap pos . (!! 3) . iterate (fmap tick) $ sys) `shouldBe` [(3,0,0),(-8,0,0)]
    describe "will invert" $ do
      it "knows which particles are going to invert" $ do
        let (Right sys) = msys
        fmap willInvert sys `shouldBe` [True,False]

parser = particle `sepBy1` newline
  where
    particle = Particle <$> ("p=" *> point <* ", ")
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

tickColliding :: Map Point (Int, Particle) -> Map Point (Int, Particle)
tickColliding = M.mapMaybe id . L.foldl' insert mempty . fmap (fmap tick) . M.elems
  where
    insert m p = M.alter (maybe (Just (Just p)) (pure (Just Nothing))) (pos $ snd p) m

tick :: Particle -> Particle
tick (Particle p v a) = Particle (translate v' p) v' a
  where v' = translate a v

willInvert :: Particle -> Bool
willInvert (Particle _ v a) = any inverting dimensions
  where
    inverting (Lens d) = (v ^. d) /= 0 && (a ^. d) /= 0 && (a ^. d < 0) /= (v ^. d < 0)


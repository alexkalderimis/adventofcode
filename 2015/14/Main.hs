{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}

import           Control.Applicative.Combinators
import           Data.Attoparsec.Text            (decimal)
import qualified Data.Attoparsec.Text            as A
import           Data.Char
import           Data.Coerce
import           Data.Function                   (on)
import qualified Data.HashMap.Strict             as M
import qualified Data.List                       as L
import           Data.Ord
import           Data.Text                       (Text)
import qualified Data.Text                       as T
import           Text.Parser.Char                (newline)

import           Elves
import           Elves.Advent

type Points = M.HashMap Name Int

type Name = Text

newtype Time = Seconds { secs :: Int }
  deriving (Show, Eq, Num, Real, Enum, Ord, Integral)

newtype Velocity = Velocity
  { kmps :: Int
  } deriving (Show, Eq, Num, Integral, Real, Ord, Enum)

newtype Distance = Kilometers { km :: Int } deriving (Show, Ord, Eq, Num)

data Reindeer = Reindeer
  { name       :: Name
  , speed      :: Velocity
  , flightTime :: Time
  , restTime   :: Time
  } deriving (Show, Eq)

periodLength :: Reindeer -> Time
periodLength = (+) <$> flightTime <*> restTime

distancePerPeriod :: Reindeer -> Distance
distancePerPeriod = distanceCovered <$> speed <*> flightTime

distanceCovered :: Velocity -> Time -> Distance
distanceCovered v t = Kilometers (kmps v * secs t)

distanceAfter :: Time -> Reindeer -> Distance
distanceAfter t r =
  let (Seconds full, rem) = quotRem t (periodLength r)
      d = distancePerPeriod r
      v = speed r
      t' = min rem (flightTime r)
   in coerce full * d + distanceCovered v t'

main :: IO ()
main = day 14 parser pt1 pt2 test
  where
    pt1 = print . maximum . fmap (distanceAfter (Seconds 2503))
    pt2 = print . L.maximumBy (comparing snd) . M.toList
                . raceWithPoints (Seconds 2503)

test = do
  let mrs = parseOnly parser exampleInput

  describe "pt1 example" $ do
    specify "Comet is in the lead after 1000s" $ do
      let winner = L.maximumBy . comparing $ distanceAfter (Seconds 1000)
      fmap (name . winner) mrs `shouldBe` Right "Comet"

  describe "pt2 example" $ do
    specify "Dancer wins after 1000s" $ do
      let points = raceWithPoints (Seconds 1000) <$> mrs
          expected = M.fromList [("Comet", 312), ("Dancer", 689)]
      points `shouldBe` Right expected

raceWithPoints :: Time -> [Reindeer] -> Points
raceWithPoints end rs = L.foldl' race mempty [1 .. end]
  where
    incr       = M.alter (pure . maybe 1 (+ 1))
    race ps    = L.foldr incr ps . inLeadAt
    inLeadAt t = fmap (name . fst)
                 . head
                 . L.groupBy ((==) `on` snd)
                 . L.sortBy (comparing (Down . snd))
                 . zip rs
                 $ fmap (distanceAfter t) rs

parser :: Parser [Reindeer]
parser = reindeer `sepBy1` newline
  where
    reindeer = do
      who <- A.takeWhile isLetter
      vel <- " can fly " *> decimal <* " km/s"
      ft  <- " for " *> decimal <* " seconds,"
      rt  <- " but then must rest for " *> decimal <* " seconds."
      return (Reindeer who vel ft rt)

exampleInput = T.unlines
  ["Comet can fly 14 km/s for 10 seconds, but then must rest for 127 seconds."
  ,"Dancer can fly 16 km/s for 11 seconds, but then must rest for 162 seconds."
  ]

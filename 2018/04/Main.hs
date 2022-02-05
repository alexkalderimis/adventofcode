{-# LANGUAGE OverloadedStrings #-}

import Control.Applicative
import Control.Arrow (second)
import Data.Functor
import Data.Maybe
import qualified Data.Text as T
import qualified Data.Ix as Ix
import qualified Data.Map.Strict as M
import Text.Printf (printf)
import qualified Data.List.Extra as L
import qualified Data.Attoparsec.Text as A
import           Control.Applicative.Combinators hiding (count)
import           Text.Parser.Char (text, newline)

import Test.QuickCheck (forAll, shuffle)

import Elves
import Elves.Advent
import qualified Elves.SeqParser as SP

type Minute = Int
type SleepPeriod = (Minute, Minute)

data TimeStamp = TimeStamp
  { tsYear :: Int
  , tsMonth :: Int
  , tsDay :: Int
  , tsHour :: Int
  , tsMinute :: Int
  } deriving (Show, Eq, Ord)

newtype GuardId = GuardId { unGuardId :: Int }
  deriving (Show, Eq, Ord)

data GuardEvent = StartShift GuardId | FallAsleep | WakeUp
  deriving (Show, Eq)

type Log = [(TimeStamp, GuardEvent)]

data Shift = Shift
  { guardId :: GuardId
  , startTime :: TimeStamp
  , sleepPeriods :: [SleepPeriod]
  } deriving (Show, Eq)

type ObservedSleep = M.Map GuardId [SleepPeriod]

main :: IO ()
main = day 4 parser pt1 pt2 spec
  where
    pt1 obs = do
      let (gid, ps) = sleepiestGuard obs
          (m, n) = sleepiestMinute ps
      printf "Guard #%d: asleep for %d minutes\n" (unGuardId gid) (totalSleep ps)
      printf "Sleepiest minute: %d (%d times)\n" m n
      solution gid m

    pt2 obs = do
      let (gid, (m, n)) = mostPredicatableSleeper obs
      printf "Guard #%d: asleep at minute %d on %d occasions\n" (unGuardId gid) m n
      solution gid m

    parser = guardLogP >>= either fail (pure . observeShifts) . buildShifts

    solution :: GuardId -> Minute -> IO ()
    solution gid m = printf "gid * min = %d\n" (unGuardId gid * m)

-- two pass-parsing (tokens->log) to deal with the sort-order without
-- doing backtracking in Attoparsec
guardLogP :: Parser Log
guardLogP = logLineP `sepBy1` newline
  where
    logLineP = do
      ts <- timeStampP
      text " "
      event <- eventP
      pure (ts, event)

buildShifts :: Log -> Either String [Shift]
buildShifts = SP.parseOnly (some p) . L.sortOn fst
  where
    p = do
      (ts, StartShift gid) <- SP.token
      ps <- many sleepPeriodP
      pure (Shift gid ts ps)
    sleepPeriodP = do
      (t0, FallAsleep) <- SP.token
      (t1, WakeUp) <- SP.token
      pure (tsMinute t0, tsMinute t1 - 1)

observeShifts :: [Shift] -> ObservedSleep
observeShifts = L.foldl' f M.empty
  where
    f m shift | null (sleepPeriods shift) = m
    f m shift = M.alter (g (sleepPeriods shift)) (guardId shift) m

    g ps Nothing = pure ps
    g ps (Just ps') = pure (ps <> ps')

timeStampP :: Parser TimeStamp
timeStampP = text "[" *> ts <* text "]"
  where
    ts = do
      [y,m,d] <- A.decimal `sepBy` text "-"
      text " "
      [hrs,min] <- A.decimal `sepBy` text ":"

      pure (TimeStamp y m d hrs min)

eventP :: Parser GuardEvent
eventP = choice [ startShift, fallAsleep, wakeUp ]
  where
    startShift = do
      gid <- text "Guard #" *> (GuardId <$> A.decimal) <* text " begins shift"
      pure (StartShift gid)
    fallAsleep = FallAsleep <$ text "falls asleep"
    wakeUp = WakeUp <$ text "wakes up"

sleepiestMinute :: [SleepPeriod] -> (Minute, Int)
sleepiestMinute ps = L.maximumOn snd $ fmap (\m -> (m, count (`Ix.inRange` m) ps)) [0..59]

sleepiestGuard :: ObservedSleep -> (GuardId, [SleepPeriod])
sleepiestGuard = L.maximumOn (totalSleep . snd) . M.toList

mostPredicatableSleeper :: ObservedSleep -> (GuardId, (Minute, Int))
mostPredicatableSleeper = L.maximumOn (snd . snd) . M.toList . M.map sleepiestMinute

totalSleep :: [SleepPeriod] -> Int
totalSleep = sum . fmap Ix.rangeSize

spec = do
  let parse = buildShifts <=< parseOnly guardLogP 
  let example = ["[1518-11-01 00:00] Guard #10 begins shift"
                ,"[1518-11-01 00:05] falls asleep"
                ,"[1518-11-01 00:25] wakes up"
                ,"[1518-11-01 00:30] falls asleep"
                ,"[1518-11-01 00:55] wakes up"
                ,"[1518-11-01 23:58] Guard #99 begins shift"
                ,"[1518-11-02 00:40] falls asleep"
                ,"[1518-11-02 00:50] wakes up"
                ,"[1518-11-03 00:05] Guard #10 begins shift"
                ,"[1518-11-03 00:24] falls asleep"
                ,"[1518-11-03 00:29] wakes up"
                ,"[1518-11-04 00:02] Guard #99 begins shift"
                ,"[1518-11-04 00:36] falls asleep"
                ,"[1518-11-04 00:46] wakes up"
                ,"[1518-11-05 00:03] Guard #99 begins shift"
                ,"[1518-11-05 00:45] falls asleep"
                ,"[1518-11-05 00:55] wakes up"
                ]

  describe "parsing" $ do
    let expected = [ Shift (GuardId 10)
                           (TimeStamp 1518 11 1 0 0)
                           [(5,24),(30,54)]
                   , Shift (GuardId 99)
                           (TimeStamp 1518 11 1 23 58)
                           [(40,49)]
                   , Shift (GuardId 10)
                           (TimeStamp 1518 11 3 0 5)
                           [(24,28)]
                   , Shift (GuardId 99)
                           (TimeStamp 1518 11 4 0 2)
                           [(36,45)]
                   , Shift (GuardId 99)
                           (TimeStamp 1518 11 5 0 3)
                           [(45,54)]
                   ]
                           
    specify "we can parse the log" $ do
      forAll (shuffle example) $ \lines -> do
        L.sort lines `shouldBe` example
        parse (T.unlines lines) `shouldBe` Right expected

  describe "pt1" $ do
    let shouldSolveCorrectly lines = do
          let (Right log) = parse (T.unlines lines)
              obs = observeShifts log
              (gid, ps) = sleepiestGuard obs

          gid `shouldBe` GuardId 10
          totalSleep ps `shouldBe` 50
          sleepiestMinute ps `shouldBe` (24, 2)

    specify "we can find the sleepiest guard" (shouldSolveCorrectly example)

    specify "we can find the sleepiest guard, no matter the input order" $
      forAll (shuffle example) shouldSolveCorrectly

  describe "mostPredicatableSleeper" $ do
    specify "we can find the most predicatable sleeper, no matter the input order" $
      forAll (shuffle example) $ \lines -> do
        let (Right log) = parse (T.unlines lines)
            obs = observeShifts log

        mostPredicatableSleeper obs `shouldBe` (GuardId 99, (45, 3))

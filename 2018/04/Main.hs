{-# LANGUAGE LambdaCase #-}

import qualified Text.ParserCombinators.ReadP as R
import Control.Applicative
import Data.Functor
import Data.Bool
import Data.Ord (comparing)
import Data.Maybe
import Data.Coerce
import Data.Monoid hiding ((<>))
import Data.Semigroup
import qualified Data.Array as A
import           Data.Array ((//))
import qualified Data.Map as M
import Text.Read (readsPrec)
import Text.Printf (printf)
import qualified Data.List as L

data LogEntry = LogEntry
  { leTimeStamp :: TimeStamp
  , leEvent :: GuardEvent
  } deriving (Show)

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
  deriving (Show)

parseLog :: String -> Maybe LogEntry
parseLog = fmap fst . listToMaybe . R.readP_to_S logEntryP

logEntryP :: R.ReadP LogEntry
logEntryP = LogEntry <$> timeStampP
                     <*> (R.char ' ' >> guardEventP)

timeStampP :: R.ReadP TimeStamp
timeStampP = R.char '[' *> ts <* R.char ']'
  where
    ts = do y <- int
            _ <- R.char '-'
            m <- int
            _ <- R.char '-'
            d <- int
            _ <- R.char ' '
            h <- int
            _ <- R.char ':'
            m' <- int
            return (TimeStamp y m d h m')

guardEventP :: R.ReadP GuardEvent
guardEventP = R.choice [shiftStart, fallAsleep, wakeUp]
  where shiftStart = StartShift <$> (R.string "Guard #" *> (GuardId <$> int) <* R.string " begins shift")
        fallAsleep = FallAsleep <$ R.string "falls asleep"
        wakeUp     = WakeUp     <$ R.string "wakes up"

int :: R.ReadP Int
int = R.readS_to_P (readsPrec 10)

type Log = [LogEntry]
type Minute = Int
type SleepPeriod = (Minute, Minute)
newtype SleepPattern = SleepPattern { unSleepPattern :: A.Array Minute Int } deriving (Show)

sleepPatternIx :: (Minute, Minute)
sleepPatternIx = (0,59)

instance Semigroup SleepPattern where
  (SleepPattern p0) <> (SleepPattern p1) = SleepPattern (p0 // [(i, a + b) | (i, b) <- A.assocs p1, b /= 0, let a = p0 A.! i ])

instance Monoid SleepPattern where
  mempty = mkPattern (pure 0)
  mappend = (<>)

mkPattern :: (Minute -> Int) -> SleepPattern
mkPattern f = SleepPattern (A.listArray sleepPatternIx (fmap f $ A.range sleepPatternIx))

sleepPeriods :: Log -> [SleepPeriod]
sleepPeriods [] = []
sleepPeriods (LogEntry{leTimeStamp = t0, leEvent = FallAsleep} : LogEntry{leTimeStamp = t1, leEvent = WakeUp} : log) = (tsMinute t0, tsMinute t1) : sleepPeriods log
sleepPeriods (_ : log) = sleepPeriods log

isAsleep :: Minute -> SleepPeriod -> Bool
isAsleep m (start, end) = m >= start && m < end

sleepPattern :: [SleepPeriod] -> SleepPattern
sleepPattern = foldMap $ \period -> mkPattern (\m -> if isAsleep m period then 1 else 0)

sleepPatterns :: Log -> M.Map GuardId SleepPattern
sleepPatterns = M.fromListWith (<>) . fmap (fmap (sleepPattern . sleepPeriods)) . groupGuard
  where
    groupGuard = \case
      [] -> []
      (e : log) -> case leEvent e of
        StartShift g -> let (shift, log') = guardShift log in (g, shift) : groupGuard log'
        _            ->  error $ "event without guard on shift: " <> show e

    guardShift = L.break (newShift . leEvent)
    newShift StartShift{} = True
    newShift _ = False

totalSleep :: SleepPattern -> Int
totalSleep = sum . A.elems . unSleepPattern

sleepiestMinute :: SleepPattern -> Int
sleepiestMinute = fst . L.maximumBy (comparing snd) . A.assocs . unSleepPattern

sleepiestGuard :: M.Map GuardId SleepPattern -> (GuardId, SleepPattern)
sleepiestGuard = L.maximumBy (comparing (totalSleep . snd)) . M.toList

mostReliable :: M.Map GuardId SleepPattern -> (GuardId, SleepPattern)
mostReliable = L.maximumBy (comparing peak) . M.toList
  where peak = safeMax . A.elems . unSleepPattern . snd
        safeMax [] = Nothing
        safeMax xs = Just (maximum xs)
   
display :: SleepPattern -> [String]
display p = ("Total Sleep: " <> show (totalSleep p))
          : ("Sleepiest minute: " <> show (sleepiestMinute p))
          : fmap displayRow (reverse [1 .. maxSleep])
  where
    displayRow lim = printf "%02d: " lim
                   <> A.elems (fmap (bool '.' 'x' . (lim <=)) (unSleepPattern p))
    maxSleep       = maximum . A.elems . unSleepPattern $ p

main :: IO ()
main = do
  log <- L.sort . lines <$> getContents
  -- mapM_ putStrLn log
  let mEvents = traverse parseLog log
  case mEvents of
    Nothing -> error "Could not parse log"
    Just es -> do let ps = sleepPatterns es
                  putStrLn "SLEEPIEST"
                  doSleepiest ps
                  putStrLn "-------------"
                  putStrLn "MOST RELIABLE"
                  doMostReliable ps
  where
    showGuard (g,p) = do
      print g
      mapM_ putStrLn (display p)
      putStrLn $ "ANSWER: " <> show (unGuardId g * sleepiestMinute p)
    doSleepiest = showGuard . sleepiestGuard
    doMostReliable = showGuard . mostReliable

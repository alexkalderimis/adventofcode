module Elves.Advent where

import           Control.Applicative
import           Control.Monad
import           Data.Attoparsec.Text (Parser, endOfInput, parseOnly)
import qualified Data.Text.IO         as Text
import qualified Data.Time.Clock      as Clock
import           System.Environment
import           System.Exit
import           Text.Parser.Char     (newline)

import           Test.Hspec

type Part a = a -> IO ()

-- | Combinator to make exercises simpler to write by encapsulating common patterns.
--
-- When starting out the following is sufficient:
--
-- @
--   main = day 1 parser pt1 pt2 test
--     where
--       parser = pure ()
--       pt1 = print
--       pt2 = print
--       test = pure ()
-- @
--
-- And then each element may be elaborated iteratively.
--
day :: Int      -- ^ The day (1..25)
    -> Parser a -- ^ A Parser for the input, received on stdin
    -> Part a   -- ^ An implementation for part-one
    -> Part a   -- ^ An implementation for part-two (available once part-one is solved)
    -> Spec     -- ^ A test suite
    -> IO ()    -- ^ The main action
day n parser pt1 pt2 spec = do
  staticDay n (getInput >>= pt1) (getInput >>= pt2) spec
  where
    getInput = Text.getContents >>= either (die . ("Could not parse input! " ++)) pure
                                    . parseOnly (parser <* (many newline >> endOfInput))

-- for exercises with simple input that can be inlined, and this is known statically
staticDay :: Int -> IO () -> IO () -> Spec -> IO ()
staticDay n pt1 pt2 spec = do
  args <- getArgs
  case args of
    ("pt1":rst)  -> withArgs rst (namedTime "pt1" pt1)
    ("pt2":rst)  -> withArgs rst (namedTime "pt2" pt2)
    ("test":test_args) -> withArgs test_args
                          $ hspec (describe ("Day " <> show n) spec)
    _        -> die "bad arguments. Expected pt1, pt2 or test"

namedTime :: String -> IO () -> IO ()
namedTime name act = do
  unless (null name) $ putStrLn name >> putStrLn (replicate (length name) '-')
  start <- Clock.getCurrentTime
  act
  unless (null name) $ putStrLn (replicate (length name) '-')
  end <- Clock.getCurrentTime
  print (Clock.diffUTCTime end start)

time :: IO () -> IO ()
time = namedTime ""

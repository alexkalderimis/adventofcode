module Elves.Advent (
  Part, day, staticDay, generalDay, namedTime, time,
  Parser, parseOnly,
  module X
  ) where

import           Control.Applicative
import           Control.Monad
import           Data.Attoparsec.Text (Parser, endOfInput, parseOnly)
import qualified Data.Text.IO         as Text
import qualified Data.Time.Clock      as Clock
import           System.Environment
import           System.Exit
import qualified Data.List.Extra as L
import           Text.Parser.Char     (newline)

import           Test.Hspec as X

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
day n parser pt1 pt2 spec = getArgs >>= runDay n parser spec [("pt1", pt1), ("pt2", pt2)]

-- for exercises with simple input that can be inlined, and this is known statically
staticDay :: Int -> IO () -> IO () -> Spec -> IO ()
staticDay n pt1 pt2 spec = getArgs >>= runDay n (pure ()) spec parts
  where
    parts = [("pt1", const pt1), ("pt2", const pt2)]

generalDay :: Int
    -> Parser a -- ^ A Parser for the input, received on stdin
    -> Spec     -- ^ A test suite
    -> [(String, Part a)] -- named parts
    -> IO ()    -- ^ The main action
generalDay n parser spec parts = getArgs >>= runDay n parser spec parts

runDay :: Int
    -> Parser a -- ^ A Parser for the input, received on stdin
    -> Spec     -- ^ A test suite
    -> [(String, Part a)] -- named parts
    -> [String] -- command line arguments
    -> IO ()    -- ^ The main action
runDay n parser spec parts args
  | ("test":test_args) <- args
  = withArgs test_args $ hspec (describe ("Day " <> show n) spec)

  | ["all"] <- args
  = do inp <- getInput
       runTests []
       forM_ parts $ \(name, part) -> putStrLn "" >> namedTime name (part inp)

  | (name:rst) <- args
  , Just part <- lookup name parts 
  = withArgs rst (namedTime name (getInput >>= part))
  
  | otherwise = badArgs
  where
    badArgs = die . unwords $ [ "bad arguments."
                              , "Expected one of"
                              , L.intercalate ", " (fst <$> parts)
                              , "or test, got:"
                              ] <> args

    runTests args = withArgs args $ hspec (describe ("Day " <> show n) spec)
    getInput = Text.getContents >>= either (die . ("Could not parse input! " ++)) pure
                                    . parseOnly (parser <* (many newline >> endOfInput))

namedTime :: String -> IO a -> IO a
namedTime name act = do
  unless (null name) $ putStrLn name >> putStrLn (replicate (length name) '-')
  start <- Clock.getCurrentTime
  r <- act
  unless (null name) $ putStrLn (replicate (length name) '-')
  end <- Clock.getCurrentTime
  print (Clock.diffUTCTime end start)
  pure r

time :: IO () -> IO ()
time = namedTime ""

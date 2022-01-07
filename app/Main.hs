{-# LANGUAGE OverloadedStrings #-}

import System.Environment
import System.Directory
import System.Exit
import System.IO
import System.Process.Typed
import Data.Maybe
import qualified Data.List as L
import qualified Data.ByteString.Lazy as BS
import Options.Applicative hiding (action)

type Cmd = ProcessConfig () () ()
data Action = Test | PT1 | PT2 deriving (Show, Eq)

data Options = Options
  { year :: String
  , day :: String
  , variant :: String
  , action :: Action
  } deriving (Show, Eq)

validDays, validYears :: [String]
validDays = (show <$> [10..25]) <> ([1..9] >>= (\n -> [show n, '0' : show n]))
validYears = ["2015", "2017", "2018", "2019", "2020", "2021"]

mkOptions :: Maybe String -> String -> String -> Maybe Action -> Options
mkOptions mvariant year day action = Options
  { year = year
  , day = day
  , variant = fromMaybe "" mvariant
  , action = fromMaybe Test action
  }

parseOpts :: Parser Options
parseOpts = mkOptions
  <$> optional (strOption (long "variant" <>
                           metavar "SUFFIX" <>
                           help "an optional variant to run"))
  <*> argument parseYear (metavar "YEAR")
  <*> argument parseDay (metavar "DAY")
  <*> optional (argument parseAction (metavar "ACTION"))

parseAction :: ReadM Action
parseAction = eitherReader $ \s -> case s of
  "pt1" -> pure PT1
  "pt2" -> pure PT2
  "test" -> pure Test
  _ -> Left ("Cannot intepret '" <> s <> "' as an action. Expected pt1, pt2, or test")

parseYear :: ReadM String
parseYear = eitherReader $ \s ->
  if s `elem` validYears
  then pure s
  else Left ("Invalid year. Expected one of " <> show validYears <> ". Got: " <> s)

parseDay :: ReadM String
parseDay = eitherReader $ \s ->
  if s `elem` validDays
  then pure (take (2 - length s) "00" <> s)
  else Left ("Invalid day: " <> s <> ". Advent runs 1-25")

main :: IO ()
main = run =<< execParser args
  where
    args = info (parseOpts <**> helper)
                (fullDesc <>
                 progDesc "Run a given puzzle solution" <>
                 header "run - puzzle runner")

run :: Options -> IO ()
run opts = do
  let executable = exe opts
  (exitCode, out, err) <- readProcess (proc "make" [executable])
  case exitCode of
    ExitSuccess -> do
      f <- inputProvider opts
      runProcess (f $ proc executable $ commandLine opts) >>= exitWith
    _ -> BS.hPut stderr err >> exitWith exitCode 

-- This value needs to be kept in sync with the build target in the Makefile and the bin/source_file script.
exe :: Options -> String
exe opts = L.intercalate "_" $ filter (not . null) ["build/puzzle", year opts, day opts, variant opts]

inputFile :: Options -> String
inputFile opts = mconcat [year opts, "/", day opts, "/input"]

inputProvider :: Options -> IO (Cmd -> Cmd)
inputProvider opts = case action opts of
  Test -> pure id
  _    -> do exists <- doesFileExist (inputFile opts)
             if exists
             then setStdin . useHandleClose <$> openFile (inputFile opts) ReadMode
             else pure id

commandLine :: Options -> [String]
-- commandLine opts = ["+RTS", "-N4", "-RTS"] ++ case action opts of
commandLine opts = case action opts of
  Test -> ["test"]
  PT1 -> ["pt1"]
  PT2 -> ["pt2"]

-- vim: syntax=haskell

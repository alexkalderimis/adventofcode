module Elves.Advent where

import           Data.Attoparsec.Text    (Parser, parseOnly, endOfInput)
import qualified Data.Text.IO            as Text
import           System.Environment
import           System.Exit

import           Test.Hspec

type Part a = a -> IO ()

day :: Int -> Parser a -> Part a -> Part a -> Spec -> IO ()
day n parser pt1 pt2 spec = do
  args <- getArgs
  case args of
    ["pt1"]  -> getInput >>= pt1
    ["pt2"]  -> getInput >>= pt2
    ("test":test_args) -> withArgs test_args
                          $ hspec (describe ("Day " ++ show n) spec)
    _        -> die "bad arguments. Expected pt1,pt2 or test"
  where
    getInput = Text.getContents >>= either (die . ("Could not parse input! " ++)) pure . parseOnly (parser <* endOfInput)



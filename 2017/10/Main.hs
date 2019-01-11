{-# LANGUAGE OverloadedStrings #-}

import qualified Data.ByteString    as B
import           Data.Foldable      hiding (toList)
import           Data.Word
import           System.Environment
import           System.Exit
import           Test.Hspec

import qualified Elves.Zipper as Z
import qualified Elves.Knot as Knot
import           Elves.Zipper (Zipper)

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["pt1"]  -> pt1
    ["pt2"]  -> pt2
    ["test"] -> withArgs [] (hspec spec)
    _        -> die "bad arguments, expected pt1, or pt2"

pt1 :: IO ()
pt1 = do
  -- this is possibly the dumbest possible way to parse input
  -- only acceptable because we know exactly what the input is.
  lens <- read . ('[':) . (++ "]") <$> getContents
  print (pinchHash 256 lens)

pt2 :: IO ()
pt2 = B.interact Knot.hash >> putStrLn ""

pinchHash :: Int -> [Int] -> Int
pinchHash 0 = error "Knot must not be empty"
pinchHash n
  = product . take 2 . Z.toList . Z.rewind . foldl' pinch' z0 . zip [0 ..]
  where
    z0 = foldr Z.insertR (Z.singleton 0) (take (n - 1) [1 ..])
    pinch' z (skip, len) = Knot.pinch len skip z

spec :: Spec
spec = do
  describe "asciiHash" $ do
    it "can hash the empty string" $ do
      Knot.hash "" `shouldBe` "a2582a3a0e66e6e86e3812dcb672a272"
    it "can hash AoC 2017" $ do
      Knot.hash "AoC 2017" `shouldBe` "33efeb34ea91902bb2f59c9920caa6cd"
    it "can hash 1,2,3" $ do
      Knot.hash "1,2,3" `shouldBe` "3efbe78a8d82f29979031a4aa0b16a9d"
    it "can hash 1,2,4" $ do
      Knot.hash "1,2,4" `shouldBe` "63960835bcdc130f0b66d7ff4f6a5a8e"


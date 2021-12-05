import qualified Data.Text as T
import           Data.Text (Text)
import qualified Data.Attoparsec.Text as A
import Text.Parser.Combinators (sepBy1)
import Text.Parser.Char (newline)

import Elves
import Elves.Advent

main :: IO ()
main = day 01 parser pt1 pt2 test
  where
    parser = A.decimal `sepBy1` newline
    pt1 = print . increases
    pt2 = print . smoothedIncreases

test = describe "2021/01" $ do
  let exampleInput = [ 199
                     , 200
                     , 208
                     , 210
                     , 200
                     , 207
                     , 240
                     , 269
                     , 260
                     , 263
                     ]
  it "counts increases correctly" $ do
    increases exampleInput `shouldBe` 7
  it "counts increases correctly, part 2" $ do
    smoothedIncreases exampleInput `shouldBe` 5

smoothedIncreases :: [Int] -> Int
smoothedIncreases = increases . map (\(a,b,c) -> a + b + c) . slidingWindows
  where
    slidingWindows (a:b:xs) = zip3 (a:b:xs) (b:xs) xs
    slidingWindows _ = []

increases :: [Int] -> Int
increases [] = 0
increases (x:xs) = length
                 . filter (uncurry (<))
                 $ zip (x:xs) xs

{-# LANGUAGE OverloadedStrings #-}

import           Control.Applicative.Combinators
import           Control.Monad
import           Data.Attoparsec.Text            (double)
import           Elves
import           Elves.Advent
import           Text.Parser.Char                (newline)

type Mass = Double
type Fuel = Int

main :: IO ()
main = day 1 (double `sepBy1` newline) pt1 pt2 test
  where
    pt1 = print . sum . fmap fuel
    pt2 = print . sum . (>>= moduleFuel)

fuel :: Mass -> Fuel
fuel = subtract 2 . floor . (/ 3)

moduleFuel :: Mass -> [Fuel]
moduleFuel = takeWhile (> 0) . fmap floor . drop 1 . iterate (fromIntegral . fuel)

test = do
  describe "fuel" $ do
    let examples = [(12.0, 2)
                   ,(14.0, 2)
                   ,(1969.0, 654)
                   ,(100756.0, 33583)
                   ]
    forM_ examples $ \(mass, expected) -> it ("gets the right result for " <> show mass) $ do
      fuel mass `shouldBe` expected
  describe "moduleFuel" $ do
    let examples = [(14.0, [2])
                   ,(1969.0, [654, 216, 70, 21, 5])
                   ,(100756, [33583, 11192, 3728, 1240, 411, 135, 43, 12, 2])
                   ]
    forM_ examples $ \(mass, expected) -> it ("gets the right result for " <> show mass) $ do
      moduleFuel mass `shouldBe` expected

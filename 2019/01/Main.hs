{-# LANGUAGE OverloadedStrings #-}

import           Control.Monad
import           Data.Function ((&))
import           Data.Maybe
import qualified Data.Text     as Text
import           Elves
import           Elves.Advent

type Mass = Double
type Fuel = Int

main :: IO ()
main = day 1 (pure ()) pt1 pt2 test
  where
    pt1 masses = print ()
    pt2 masses = print ()

fuel :: Mass -> Fuel
fuel = subtract 2 . floor . (/ 3)

test = do
  describe "fuel" $ do
    let examples = [(12.0, 2)
                   ,(14.0, 2)
                   ,(1969, 654)
                   ,(100756, 33583)
                   ]
    forM_ examples $ \(mass, expected) -> it ("gets the right result for " <> show mass) $ do
      fuel mass `shouldBe` expected

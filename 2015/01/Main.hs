{-# LANGUAGE OverloadedStrings #-}

import           Prelude       hiding (floor)

import           Control.Monad
import           Data.Function ((&))
import qualified Data.List     as L
import           Data.Maybe
import qualified Data.Text     as Text
import           Elves
import           Elves.Advent

type Direction = Int -> Int

main :: IO ()
main = day 1 (some direction) pt1 pt2 test
  where
    pt1 ds = print (floor ds 0)
    pt2 ds = print (entersBasementAt ds)

floor :: [Direction] -> Int -> Int
floor = L.foldl' (.) id

direction :: Parser Direction
direction = (succ <$ "(") <|> (pred <$ ")")

entersBasementAt :: [Direction] -> Maybe Int
entersBasementAt = listToMaybe . fmap fst . dropWhile ((>= 0) . snd) . zip [0 ..] . L.scanl (&) 0

test = do
  describe "entersBasementAt" $ do
    let examples = [(")", Just 1)
                   ,("()())", Just 5)
                   ,("(((", Nothing)
                   ]
    forM_ examples $ \(inp, ret) -> it ("gets the right result for " ++ Text.unpack inp) $ do
      parseOnly (entersBasementAt <$> some direction) inp `shouldBe` Right ret
  describe "floor" $ do
    let examples = [("(())", 0)
                   ,("()()", 0)
                   ,("(((", 3)
                   ,("(()(()(", 3)
                   ,("))(((((", 3)
                   ,("())", -1)
                   ,("))(", -1)
                   ,(")))", -3)
                   ,(")())())", -3)
                   ]
    forM_ examples $ \(inp, ret) -> it ("gets the right result for " ++ Text.unpack inp) $ do
      parseOnly (flip floor 0 <$> some direction) inp `shouldBe` Right ret

{-# LANGUAGE OverloadedStrings #-}

import           Control.Applicative.Combinators
import           Data.Attoparsec.Text            (decimal, signed)
import           Text.Parser.Char                (newline)

import qualified Data.Attoparsec.Text            as A
import qualified Data.HashMap.Strict             as M
import           Data.Text                       (Text)
import qualified Data.Text                       as T

import           Elves
import           Elves.Advent

-- degenerate form of JSON.
-- Implemented here instead of using Aeson, well, because this is
-- a set of coding exercises.
data ElvenJSON
  = Object (M.HashMap Text ElvenJSON)
  | Array [ElvenJSON]
  | JString Text -- elven strings are very simple - no full on escaping
  | JInt Int   -- in elven JSON land, all numbers are ints
  | JBool Bool -- not actually in the input
  | JNull      -- not found in the input
  deriving (Show, Eq)

main :: IO ()
main = day 12 parser pt1 pt2 test
  where
    parser = myDegenerateJSONParser
    pt1 = print . sum . ints (pure False)
    pt2 = print . sum . ints taggedRed

test = do
  describe "pt1" $ do
    let examples = [("[1,2,3]", 6)
                   ,("{\"a\":2,\"b\":4}", 6)
                   ,("[[[3]]]", 3)
                   ,("{\"a\":{\"b\":4},\"c\":-1}", 3)
                   ,("{\"a\":[-1,1]}", 0)
                   ,("[-1,{\"a\":1}]", 0)
                   ,("[]", 0)
                   ,("{}", 0)
                   ]
    forM_ examples $ \(raw, sumOfInts) -> do
      describe (T.unpack raw) $ do
        let mr = parseOnly myDegenerateJSONParser raw
        specify (unwords ["the sum of ints is", show sumOfInts]) $ do
          fmap (sum . ints (pure False)) mr `shouldBe` Right sumOfInts
  describe "pt2" $ do
    let examples = [("[1,2,3]", 6)
                   ,("{\"a\":2,\"b\":4}", 6)
                   ,("[[[3]]]", 3)
                   ,("{\"a\":{\"b\":4},\"c\":-1}", 3)
                   ,("{\"a\":[-1,1]}", 0)
                   ,("[-1,{\"a\":1}]", 0)
                   ,("[]", 0)
                   ,("{}", 0)
                   ,("[1,{\"c\":\"red\",\"b\":2},3]", 4)
                   ,("{\"d\":\"red\",\"e\":[1,2,3,4],\"f\":5}", 0)
                   ,("[1,\"red\",5]", 6)
                   ]
    forM_ examples $ \(raw, sumOfInts) -> do
      describe (T.unpack raw) $ do
        let mr = parseOnly myDegenerateJSONParser raw
        specify (unwords ["the sum of ints is", show sumOfInts]) $ do
          fmap (sum.ints taggedRed) mr `shouldBe` Right sumOfInts

ints :: (M.HashMap Text ElvenJSON -> Bool) -> ElvenJSON -> [Int]
ints ignored val = case val of
  Object m | ignored m -> []
  Object m -> M.elems m >>= ints ignored
  Array vs -> vs >>= ints ignored
  JInt i   -> pure i
  _        -> []

taggedRed :: M.HashMap Text ElvenJSON -> Bool
taggedRed = any (== JString "red") . M.elems

myDegenerateJSONParser :: Parser ElvenJSON
myDegenerateJSONParser = A.skipSpace *> value
  where
    obj = between ("{" *> A.skipSpace) (A.skipSpace *> "}")
                  (fmap M.fromList (pair `sepBy` comma))
    arr = between ("[" *> A.skipSpace) (A.skipSpace *> "]")
                  (value `sepBy` comma)
    pair = key <#> value
    comma = A.skipSpace *> A.string "," *> A.skipSpace
    key = between (A.skipSpace *> A.char '\"')
                  (A.char '\"' *> A.skipSpace *> A.char ':' *> A.skipSpace)
                  (A.takeWhile (/= '\"'))
    value = choice [Object <$> obj, Array <$> arr, atom]
    atom = choice [JNull <$ A.string "null"
                  ,JBool True <$ A.string "true"
                  ,JBool False <$ A.string "false"
                  ,JInt <$> signed decimal
                  ,between (A.char '\"') (A.char '\"')
                           (JString <$> A.takeWhile (/= '\"'))
                  ]

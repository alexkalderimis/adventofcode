{-# LANGUAGE OverloadedStrings #-}

import Data.Tuple (swap)
import qualified Data.List.Extra as L
import qualified Data.Text as T
import           Data.Text (Text)
import qualified Data.Attoparsec.Text as A
import qualified Data.Map.Strict as M
import           Data.Map.Strict (Map)
import           Control.Applicative.Combinators
import           Data.Attoparsec.Text            (letter)
import           Text.Parser.Char (newline, text)
import qualified Data.Vector.Unboxed as V
import           Data.Vector.Unboxed (Vector)

import Debug.Trace

import Elves
import Elves.Advent
import qualified Elves.CountMap as CM
import           Elves.CountMap (CountMap(..))

type Element = Char
newtype Polymer = Poly { getPoly :: Vector Element } deriving (Eq, Show)
data InsertionRules = IR { getRules :: Map (Element, Element) (Vector Element) }

main :: IO ()
main = day 14 parser pt1 pt2 test
  where
    pt1 (p, rs) = let after_step_10 = iterate (runRules rs) p !! 10
                   in print (strength after_step_10)
    pt2 (p, rs) = let cm = runLarge 20 4 5 rs p
                      Just (min, max) = minmax . fmap swap $ CM.counts cm
                   in print (fst max - fst min)
    strength (Poly es) = let cm = CM.fromList (V.toList es)
                             Just (min, max) = minmax . fmap swap $ CM.counts cm
                          in fst max - fst min

runRules :: InsertionRules -> Polymer -> Polymer
runRules (IR m) (Poly es) = Poly . V.force . V.concatMap replace $ V.zip es (V.tail es <> one '-')
  where one = V.singleton
        replace k = case M.lookup k m of
                        Nothing -> one (fst k)
                        Just c  -> one (fst k) <> c

magnifyRules :: Int -> InsertionRules -> InsertionRules
magnifyRules n rs = IR $ M.fromList $ do
  (a, b) <- M.keys (getRules rs)
  let after_n = getPoly $ iterate (runRules rs) (Poly $ V.fromList [a, b]) !! n
      middle = V.tail (V.init after_n)
  pure ((a, b), middle)

runCounting :: Int -> InsertionRules -> Polymer -> CountMap Element
runCounting n rs (Poly es) = L.foldl' g mempty keys
  where
    keys = zip (V.toList es) (tail (V.toList es) <> pure '-')
    memo = M.fromList [(k, f k) | k <- L.nubOrd keys]
    f (a, b) = CM.dec b . CM.fromList . V.toList . getPoly
               $ iterate (runRules rs) (Poly $ V.fromList [a, b]) !! n
    g m k = m <> (memo M.! k)

-- make use of multiple strategies to cut the time down:
-- jump: number of iterations to jump forward. This should be large enough to generate a
--       lot of pairs
-- m: magnify rules by this much
-- n: do the final calculation this many times
--
-- total iterations equals jump + (m * n)
--
runLarge :: Int -> Int -> Int -> InsertionRules -> Polymer -> CountMap Element
runLarge jump m n rs p = let p' = iterate (runRules rs) p !! jump
                             rs' = magnifyRules m rs
                          in runCounting n rs' p'

test = do
  let poly = Poly . V.fromList
  let exampleInput = [ "NNCB"
                     , ""
                     , "CB -> H"
                     , "CC -> N"
                     , "CH -> B"
                     , "CN -> C"

                     , "HB -> C"
                     , "HC -> B"
                     , "HH -> N"
                     , "HN -> C"

                     , "NB -> B"
                     , "NC -> B"
                     , "NH -> C"
                     , "NN -> C"

                     , "BB -> N"
                     , "BC -> B"
                     , "BH -> H"
                     , "BN -> B"
                     ]
  let Right (p, rs) = parseOnly parser (T.intercalate "\n" exampleInput)

  describe "runRules" $ do
    it "can run the rules correctly" $ do
      runRules rs p `shouldBe` poly "NCNBCHB"
    it "can run rules several times" $ do
      take 5 (iterate (runRules rs) p) `shouldBe` [ p
                                                  , poly "NCNBCHB"
                                                  , poly "NBCCNBBBCBHCB"
                                                  , poly "NBBBCNCCNBBNBNBBCHBHHBCHB"
                                                  , poly "NBBNBNBBCCNBCNCCNBBNBBNBBBNBBNBBCBHCBHHNHCBBCBHCB"
                                                  ]
    describe "after step 10" $ do
      let Poly after_step_10 = iterate (runRules rs) p !! 10
      it "has the correct length" $ do
        V.length after_step_10 `shouldBe` 3073
      it "has the correct counts" $ do
        let cm = CM.fromList (V.toList after_step_10)
        CM.countOf 'B' cm `shouldBe` 1749
        CM.countOf 'C' cm `shouldBe` 298
        CM.countOf 'H' cm `shouldBe` 161
        CM.countOf 'N' cm `shouldBe` 865
      it "can find the min and the max" $ do
        let cm = CM.fromList (V.toList after_step_10)
            Just (min, max) = minmax . fmap swap $ CM.counts cm
        min `shouldBe` (161, 'H')
        max `shouldBe` (1749, 'B')
    describe "it can proceed in larger steps" $ do
      let bigRules = magnifyRules 5 rs
      let Poly after_step_10 = iterate (runRules bigRules) p !! 2
      it "has the correct length" $ do
        V.length after_step_10 `shouldBe` 3073
  describe "runCounting" $ do
    it "counts correctly" $ do
      let cm = runCounting 10 rs p
      sum (M.elems $ countMap cm) `shouldBe` 3073
      CM.countOf 'B' cm `shouldBe` 1749
      CM.countOf 'C' cm `shouldBe` 298
      CM.countOf 'H' cm `shouldBe` 161
      CM.countOf 'N' cm `shouldBe` 865
    it "counts correctly with jump" $ do
      let p' = iterate (runRules rs) p !! 5
          cm = runCounting 5 rs p'
      sum (M.elems $ countMap cm) `shouldBe` 3073
      CM.countOf 'B' cm `shouldBe` 1749
      CM.countOf 'C' cm `shouldBe` 298
      CM.countOf 'H' cm `shouldBe` 161
      CM.countOf 'N' cm `shouldBe` 865
  describe "pt2" $ do
    it "can calculate the result in a reasonable time" $ do
      let cm = runLarge 20 4 5 rs p
          Just (min, max) = minmax . fmap swap $ CM.counts cm
      fst min `shouldBe` 3849876073
      fst max `shouldBe` 2192039569602
      fst max - fst min `shouldBe` 2188189693529

parser :: Parser (Polymer, InsertionRules)
parser = do
  p <- Poly . V.fromList <$> some letter
  newline >> newline
  rs <- IR . M.fromList <$> sepBy1 ruleP newline
  pure (p, rs)
  where
    ruleP = do a <- letter
               b <- letter
               text " -> "
               c <- letter
               pure ((a, b), V.singleton c)


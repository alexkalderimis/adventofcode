{-# LANGUAGE OverloadedStrings #-}

import qualified Data.List as L
import Data.List ((\\), foldl')
import qualified Data.Map.Strict as M
import           Data.Map.Strict (Map)
import qualified Data.Set as Set
import           Data.Set (Set)
import qualified Data.Attoparsec.Text as A
import Text.Parser.Combinators (sepBy1, choice, count)
import Text.Parser.Char (newline, space, text)

import Elves
import Elves.Advent

---   0:      1:      2:      3:      4:       5:      6:      7:      8:      9:
---  aaaa    ....    aaaa    aaaa    ....     aaaa    aaaa    aaaa    aaaa    aaaa
--- b    c  .    c  .    c  .    c  b    c   b    .  b    .  .    c  b    c  b    c
--- b    c  .    c  .    c  .    c  b    c   b    .  b    .  .    c  b    c  b    c
---  ....    ....    dddd    dddd    dddd     dddd    dddd    ....    dddd    dddd
--- e    f  .    f  e    .  .    f  .    f   .    f  e    f  .    f  e    f  .    f
--- e    f  .    f  e    .  .    f  .    f   .    f  e    f  .    f  e    f  .    f
---  gggg    ....    gggg    gggg    ....     gggg    gggg    ....    gggg    gggg
-- segment length:                                
--             2      
--                                                              3 
--                                     4
--                    5        5               5
--    6                                                 6                       6
--                                                                      7

data Segment = A | B | C | D | E | F | G deriving (Show, Eq, Ord)
newtype SegmentPattern = SP { segments :: Set Segment } deriving (Eq, Ord)

instance Show SegmentPattern where
  show = (>>= show) . Set.elems . segments

segmentPattern :: [Segment] -> SegmentPattern
segmentPattern = SP . Set.fromList

contains :: SegmentPattern -> SegmentPattern -> Bool
contains (SP a) (SP b) = Set.isSubsetOf b a

commonSegments :: SegmentPattern -> SegmentPattern -> SegmentPattern
commonSegments (SP a) (SP b) = SP $ Set.intersection a b

segmentLength :: SegmentPattern -> Int
segmentLength = Set.size . segments

data Display = Display SegmentPattern SegmentPattern SegmentPattern SegmentPattern
data Clue = Clue { patterns :: [SegmentPattern], display :: Display }
data Solution = Solved { dictionary :: Map SegmentPattern Word, displayedValue :: Word }

main :: IO ()
main = day 08 parser pt1 pt2 test
  where
    parser = clueP `sepBy1` newline
    pt1 = print . length . (>>= knownNumbers . digits . display)
    pt2 = print . sum . fmap (displayedValue . solve)

clueP :: Parser Clue
clueP = do
  pats <- count 10 (segmentPatternP <* space)
  text "|"
  [a, b, c, d] <- count 4 (space *> segmentPatternP)
  pure (Clue pats (Display a b c d))

segmentPatternP :: Parser SegmentPattern
segmentPatternP = fmap segmentPattern . some $ A.choice [ A <$ text "a"
                                                        , B <$ text "b"
                                                        , C <$ text "c"
                                                        , D <$ text "d"
                                                        , E <$ text "e"
                                                        , F <$ text "f"
                                                        , G <$ text "g"
                                                        ]

test = do
  let input = ["be cfbegad cbdgef fgaecd cgeb fdcge agebfd fecdb fabcd edb | fdgacbe cefdb cefbgd gcbe"
              ,"edbfga begcd cbg gc gcadebf fbgde acbgfd abcde gfcbed gfec | fcgedb cgb dgebacf gc"
              ,"fgaebd cg bdaec gdafb agbcfd gdcbef bgcad gfac gcb cdgabef | cg cg fdcagb cbg"
              ,"fbegcd cbd adcefb dageb afcb bc aefdc ecdab fgdeca fcdbega | efabcd cedba gadfec cb"
              ,"aecbfdg fbg gf bafeg dbefa fcge gcbea fcaegb dgceab fcbdga | gecf egdcabf bgf bfgea"
              ,"fgeab ca afcebg bdacfeg cfaedg gcfdb baec bfadeg bafgc acf | gebdcfa ecba ca fadegcb"
              ,"dbcfg fgd bdegcaf fgec aegbdf ecdfab fbedc dacgb gdcebf gf | cefg dcbef fcge gbcadfe"
              ,"bdfegc cbegaf gecbf dfcage bdacg ed bedf ced adcbefg gebcd | ed bcgafe cdgba cbgef"
              ,"egadfb cdbfeg cegd fecab cgb gbdefca cg fgcdab egfdb bfceg | gbdfcae bgc cg cgb"
              ,"gcafb gcf dcaebfg ecagb gf abcdeg gaef cafbge fdbac fegbdc | fgae cfgab fg bagce"
              ]
      Right clues = mapM (parseOnly clueP) input

  describe "parsing" $ do
    it "parses correctly" $ do
      let (c : _) = clues
          Display _ _ _ s = display c
      s `shouldBe` segmentPattern [G, C, B, E]

  describe "pt1 - simple counting" $ do
    it "can generate candidates from segments" $ do
      let Display a b c d = display (head clues)
      numbers a `shouldBe` [8]
      numbers b `shouldBe` [2, 3, 5]
      numbers c `shouldBe` [0, 6, 9]
      numbers d `shouldBe` [4]

    it "can match up segments to known numbers" $ do
      let ps = digits . display $ head clues
      knownNumbers ps `shouldBe` [(segmentPattern [F, D, G, A, C, B, E], 8)
                                 ,(segmentPattern [G, C, B, E], 4)
                                 ]

    it "can determine each clues known numbers" $ do
      fmap (length.knownNumbers.digits.display) clues `shouldBe` [2, 3, 3, 1, 3, 4, 3, 1, 4, 2]

    it "knows there are 26 known numbers" $ do
      length (clues >>= knownNumbers.digits.display) `shouldBe` 26

  describe "solving clues" $ do
    let clueText = "acedgfb cdfbe gcdfa fbcad dab cefabd cdfgeb eafb cagedb ab | cdfeb fcadb cdfeb cdbaf"
        Right clue = parseOnly clueP clueText
    it "can determineNumbers for clues" $ do
      let p t = let Right sp = parseOnly segmentPatternP t in sp

      determineNumbers (patterns clue) `shouldBe` M.fromList [(p "acedgfb", 8)
                                                             ,(p "cdfbe", 5)
                                                             ,(p "gcdfa", 2)
                                                             ,(p "fbcad", 3)
                                                             ,(p "dab", 7)
                                                             ,(p "cefabd", 9)
                                                             ,(p "cdfgeb", 6)
                                                             ,(p "eafb", 4)
                                                             ,(p "cagedb", 0)
                                                             ,(p "ab", 1)
                                                             ]
    it "can solve clues" $ do
      displayedValue (solve clue) `shouldBe` 5353

    it "can solve all clues" $ do
      fmap (displayedValue . solve) clues `shouldBe` [8394, 9781, 1197, 9361, 4873, 8418, 4548, 1625, 8717, 4315]

knownNumbers :: [SegmentPattern] -> [(SegmentPattern, Word)]
knownNumbers ps = [(sp, i) | sp <- ps, [i] <- [numbers sp]]

digits :: Display -> [SegmentPattern]
digits (Display a b c d) = [a, b, c, d]

numbers :: SegmentPattern -> [Word]
numbers ps = case segmentLength ps of
  2 -> [1]
  3 -> [7]
  4 -> [4]
  5 -> [2, 3, 5]
  6 -> [0, 6, 9]
  7 -> [8]
  n -> error ("Unexpected input: n = " <> show n)

solve :: Clue -> Solution
solve (Clue ps (Display a b c d)) = let dict = determineNumbers ps
                                        value = sum [1000 * (dict M.! a)
                                                    , 100 * (dict M.! b)
                                                    ,  10 * (dict M.! c)
                                                    ,       (dict M.! d)
                                                    ]
                                    in Solved dict value

-- this function makes a LOT of assumptions, encoded in the partial pattrn matches and uses of head.
-- It requires well formed input to work.
determineNumbers :: [SegmentPattern] -> Map SegmentPattern Word
determineNumbers ps = invert $ foldl' runStep find1478 [sixers, fivers]
  where
    runStep m step = M.union m . M.fromList $ step m
    invert m = M.fromList [(v, k) | (k, v) <- M.toList m]
    -- index the patterns by size
    bySize = M.fromListWith (<>) [(segmentLength sp, [sp]) | sp <- ps]

    -- step one, find the numbers with patterns of unique lengths
    find1478 = invert . M.fromList $ knownNumbers ps
      
    -- solve the sixer group:
    --
    -- includes? | nine | zero | six 
    -- -----------------------------
    -- 4         |  x   |  -   |  -  
    -- 1         |  x   |  x   |  -  
    sixers known = let one = known M.! 1
                       four = known M.! 4
                       (nine, zero, six) = doublePartition (bySize M.! 6) (`contains` four) (`contains` one)
                   in [(0, zero), (6, six), (9, nine)]

    -- Solve the fiver group
    --
    -- includes? | three | two | five 
    -- -------------------------------
    -- 1         |  x    |  -  |  -   
    -- C         |  x    |  x  |  -   
    fivers known = let one = known M.! 1
                       six = known M.! 6
                       shared = commonSegments one six -- the part of one shared with six
                       (three, five, two) = doublePartition (bySize M.! 5) (`contains` one) (`contains` shared)
                   in [(2, two), (5, five), (3, three)]

    doublePartition candidates f g = let ([a], ([b], [c])) = L.partition g <$> L.partition f candidates
                                      in (a, b, c)

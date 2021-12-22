{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}

import           Control.Comonad
import           Control.Comonad.Store
import           Data.Bool
import           Data.Array (Array, Ix)
import qualified Data.Array as A
import qualified Data.HashMap.Strict          as HM
import qualified Data.HashSet                 as S
import           Data.List                    (dropWhileEnd)
import           Data.Text                    (Text)
import qualified Data.Text                    as Text
import qualified Data.Text.IO                 as Text
import qualified Data.List                    as L

import           Text.Parser.Combinators      (choice, sepBy1, sepByNonEmpty)
import           Text.Read                    (readMaybe)
import           Text.Parser.Char (char, text, newline)

import Test.QuickCheck.Arbitrary (Arbitrary (arbitrary))
import Test.QuickCheck (chooseInt, property, NonEmptyList(..))

import Elves
import Elves.Advent (day)
import qualified Elves.Array.InfiniteCursor as C
import           Elves.Array.InfiniteCursor (Cursor)

type Pot = Bool
type Pattern = (Pot, Pot, Pot, Pot, Pot)
data PlantRule = PlantRule { patt :: !Pattern, ret :: !Pot }
  deriving (Eq, Show)

instance Arbitrary PlantRule where
  arbitrary = PlantRule <$> arbitrary <*> arbitrary

type Rule a b = a -> a -> a -> a -> a -> b

type PlantState = Cursor Int Pot

newtype PS = PS PlantState deriving (Show)

instance Arbitrary PS where
  arbitrary = do
    xs <- getNonEmpty <$> arbitrary
    let bs = (0, length xs - 1)
    i <- chooseInt bs
    pure . PS . seek i . mkState $ A.listArray bs xs

mkState :: Array Int Pot -> PlantState
mkState = C.cursor False

-- shift the entire array so that it starts at a new offset
move :: Int -> PlantState -> PlantState
move n ps = let arr    = C.toArray ps
                (i, j) = A.bounds arr
                arr'   = A.listArray (i + n, j + n) (A.elems arr)
             in seeks (+ n) $ ps { C.toArray = arr' }

main :: IO ()
main = day 12 inputP pt1 pt2 test
  where
    pt1 = getFingerPrint 20
    pt2 = assumingCycle 50000000000

    -- assumes there is a cycle - this is needed to complete pt 2
    assumingCycle n (s, rules) = do
      let c              = detectCycle s rules
          stepsRemaining = (n - cycleStart c) `div` cycleLength c
          fp             = cycleFP0 c + cycleFPD c * stepsRemaining
          remainingSteps = (n - cycleStart c) `rem` (cycleLength c)
      putStrLn $ mconcat [ "Found cycle "
                         , show (cycleStart c)
                         , ".."
                         , show (cycleStart c + cycleLength c)
                         ]
      if remainingSteps == 0
        then print fp -- great, print and go
        -- we need to fast-fwd to the end of the last cycle we can use
        -- and then run the sim from there.
        else let s = fromCycle remainingSteps c
                 gen = runRulesA rules remainingSteps s
              in print (fingerprint gen)

    -- assumes there is no cycle
    getFingerPrint n (s,rules) = do
      let gen = runRulesA rules n s

      Text.putStrLn (showState gen)
      print (fingerprint gen)

-- get the PlantState after n cycles, starting from where this cycle begins
fromCycle :: Int -> Cycle -> PlantState
fromCycle n c = let ps = cycleKey c
                    i = cycleOffset c + (n * cycleOffsetDX c)
                in move i ps

data Cycle = Cycle
  { cycleKey      :: !PlantState
  , cycleStart    :: !Int -- first step number in the cycle
  , cycleLength   :: !Int -- number of steps in the cycle
  , cycleFP0      :: !Int -- fingerprint at start of cycle
  , cycleFPD      :: !Int -- change in fingerprint over the course of the cycle
  , cycleOffset   :: !Int -- index of leftmost full pot
  , cycleOffsetDX :: !Int -- change in offset over the course of the cycle
  } deriving (Show)

-- pt 2 requires using the fact that the rules build cycles that can be
-- exploited to calculate ahead to the answer. This detects the first cycle
-- we encounter.
detectCycle :: PlantState -> [PlantRule] -> Cycle
detectCycle s rules = go init 1 s
  where
    k = showState
    r = compileRules rules
    init = HM.singleton (k s) (memo 0 s)
    memo n s = (n, fingerprint s, leftMostPot s)
    go !m !n s = let s' = stepPlantState r s
                     key = k s'
                  in case HM.lookup key m of
                     Nothing -> let m' = HM.insert key (memo n s') m
                                 in go m' (n + 1) s'
                     Just (step, fp, os) -> Cycle { cycleKey = s'
                                                  , cycleStart = step
                                                  , cycleLength = (n - step)
                                                  , cycleFP0 = fp
                                                  , cycleFPD = (fingerprint s' - fp)
                                                  , cycleOffset = os
                                                  , cycleOffsetDX = (leftMostPot s' - os)
                                                  }

inputP :: Parser (PlantState, [PlantRule])
inputP = (,) <$> initialStateP <*> (newline >> newline >> sepBy1 ruleP newline)

leftMostPot :: PlantState -> Int
leftMostPot = fst . A.bounds . C.toArray

initialStateP :: Parser PlantState
initialStateP = do
  text "initial state: "
  xs <- some plantP
  pure . mkState $ A.listArray (0, length xs - 1) xs

plantP :: Parser Bool
plantP = choice [True <$ char '#', False <$ char '.']

ruleP :: Parser PlantRule
ruleP = do
  l1 <- plantP
  l0 <- plantP
  c  <- plantP
  r0 <- plantP
  r1 <- plantP
  text " => "
  ret <- plantP
  pure (PlantRule (l1, l0, c, r0, r1) ret)

compileRules :: [PlantRule] -> Rule Bool Bool
compileRules rs =
  let patterns = buildPatterns rs
   in \a b c d e -> S.member (a, b, c, d, e) patterns

-- Array based cellular automoton, rather than the more elegant
-- Zipper based co-monadic one.
-- This is however much more memory efficient (down from 6GB+ to staying
-- under 3MB). It is also much much faster, capable of processing 1,000,000
-- steps in 45sec. Still too slow for billions of steps, though.
runRulesA :: [PlantRule] -> Int -> PlantState -> PlantState
runRulesA rules = let r = compileRules rules
                      go n ps = case n of
                                  0 -> ps
                                  n -> go (n - 1) (stepPlantState r ps)
                   in go

-- avoids growing the array
stepPlantState :: Rule Bool Bool -> PlantState -> PlantState
stepPlantState r ps = mkState . fromMaybe emptyState $ do
  a <- L.find val [i - 2, i - 1 .. j]
  b <- L.find val [j + 2, j + 1 .. i]

  pure $ A.array (a, b) [(i, val i) | i <- A.range (a, b)]
 where
   emptyState = A.listArray (0, 0) [False]
   (i,j) = A.bounds (C.toArray ps)
   val i = r (peek (i - 2) ps)
             (peek (i - 1) ps)
             (peek i ps)
             (peek (i + 1) ps)
             (peek (i + 2) ps)

-- append two pots on either side
grow :: PlantState -> PlantState
grow c = let ext = [False, False]
             arr = C.toArray c
             (a, b) = A.bounds arr
             a' = A.listArray (a - 2, b + 2) (ext <> A.elems arr <> ext)
          in c { C.toArray = a' }

trim :: PlantState -> PlantState
trim s = let pots = C.toArray s
             (lo, hi) = A.bounds pots
             a = fromMaybe 0 . L.find (pots A.!) $ [lo .. 0]
             b = fromMaybe 0 . L.find (pots A.!) $ [hi, hi - 1 .. 0]
          in s { C.toArray = A.listArray (a, b) [pots A.! i | i <- A.range (a, b)] }

runRules :: [PlantRule] -> PlantState -> [PlantState]
runRules rules = let r = compileRules rules in iterate (stepState r)

buildPatterns :: [PlantRule] -> S.HashSet Pattern
buildPatterns rs = S.fromList [ patt r | r <- rs, ret r ]

showState :: PlantState -> Text
showState = Text.pack . fmap showPlant . A.elems . C.toArray

showRule :: PlantRule -> String
showRule r = fmap showPlant [a, b, c, d, e] ++ " => " ++ [showPlant (ret r)]
  where (a, b, c, d, e) = patt r

showPlant :: Bool -> Char
showPlant = bool '.' '#'

applyRule :: (Enum i, Ix i) => Rule a b -> Cursor i a -> Cursor i b
applyRule r = extend (rule r)

stepState :: Rule Bool Bool -> PlantState -> PlantState
stepState r = trim . applyRule r . grow

-- the cursor based rule evaluator.
rule :: (Enum i, Ix i) => Rule a b -> Cursor i a -> b
rule r s = r (select (pred . pred))
             (select pred)
             (extract s)
             (select succ)
             (select (succ . succ))
  where select move = extract (seeks move s)

fingerprint :: PlantState -> Int
fingerprint s = sum [i | (i, True) <- A.assocs (C.toArray s)]

exampleInput :: Text
exampleInput = Text.unlines
  [ "initial state: #..#.#..##......###...###"
  , ""
  , "...## => #" -- rule 0.
  , "..#.. => #" -- rule 1.
  , ".#... => #" -- rule 2.
  , ".#.#. => #" -- rule 3.
  , ".#.## => #" -- rule 4.
  , ".##.. => #" -- rule 5.
  , ".#### => #" -- rule 6.
  , "#.#.# => #" -- rule 7.
  , "#.### => #" -- rule 8.
  , "##.#. => #" -- rule 9.
  , "##.## => #" -- rule 10.
  , "###.. => #" -- rule 11.
  , "###.# => #" -- rule 12.
  , "####. => #" -- rule 13.
  ]

test :: Spec
test = do
  let unzip = A.assocs . C.toArray

  describe "inputP" $ do
    let mInp = parseOnly inputP exampleInput
    it "has the correct number of rules" $ do
      fmap (length . snd) mInp `shouldBe` Right 14
    it "has the correct initial state" $ do
      let state = "#..#.#..##......###...###"
      fmap (showState . fst) mInp `shouldBe` Right state
    it "has rule 7 correct" $ do
      let rule = "#.#.# => #" -- rule 7.
      fmap (showRule . (!! 7) . snd) mInp `shouldBe` Right rule

  describe "State" $ do
    it "can parse and serialise correctly" $ property $ \(PS s)  ->
      let serialized = "initial state: " <> showState s
       in parseOnly initialStateP serialized `shouldBe` Right (seek 0 s)

  describe "PlantRule" $ do
    it "can parse and serialise correctly" $ do
      property $ \r -> parseOnly ruleP (Text.pack $ showRule r) `shouldBe` Right r

  describe "applyRule" $ do
    let (Right (state, rules)) = parseOnly inputP exampleInput
        rule = compileRules rules
        run 0 s = trim s
        run n s = run (n - 1) (stepState rule s)
    it "should step correctly from state 0 -> state 1" $ do
      showState (run 1 state) `shouldBe` "#...#....#.....#..#..#..#"
    it "should step correctly from state 0 -> state 2, growing the state" $ do
      showState (run 2 state) `shouldBe` "##..##...##....#..#..#..##"
    it "should step correctly from state r -> state 3, growing the state" $ do
      showState (run 3 state) `shouldBe` "#.#...#..#.#....#..#..#...#"

  describe "stepPlantState" $ do
    let (Right (state, rules)) = parseOnly inputP exampleInput
        rule = compileRules rules
        run n s = iterate (stepPlantState rule) s !! n
    it "should step correctly from state 0 -> state 1" $ do
      showState (run 1 state) `shouldBe` "#...#....#.....#..#..#..#"
    it "should step correctly from state 0 -> state 2, growing the state" $ do
      showState (run 2 state) `shouldBe` "##..##...##....#..#..#..##"
    it "should step correctly from state r -> state 3, growing the state" $ do
      showState (run 3 state) `shouldBe` "#.#...#..#.#....#..#..#...#"

  describe "runRules" $ do
    let (Right (state, rules)) = parseOnly inputP exampleInput
    it "should step correctly from state r -> state 3, growing the state" $ do
      let s = runRules rules state !! 3
      showState (trim s) `shouldBe` "#.#...#..#.#....#..#..#...#"
    it "can proceed to state 20, as per the example" $ do
      let s = runRules rules state !! 20
          Right asExpected = parseOnly (some plantP) "#....##....#####...#######....#.#..##"
      unzip (trim s) `shouldBe` zip [-2 .. ] asExpected

  describe "potFingerprint" $ do
    it "gets the right fingerprint for generation 20" $ do
      let (Right (state, rules)) = parseOnly inputP exampleInput
          s = runRules rules state !! 20
      fingerprint s `shouldBe` 325

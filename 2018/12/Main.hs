{-# LANGUAGE OverloadedStrings #-}

import           Control.Applicative
import           Control.Comonad
import           Control.Monad
import           Data.Bool
import qualified Data.Array.Unboxed as A
import qualified Data.HashMap.Strict          as HM
import qualified Data.HashSet                 as S
import           Data.List                    (dropWhileEnd)
import           Data.List.NonEmpty           (NonEmpty ((:|)))
import qualified Data.List.NonEmpty           as NE
import           Data.Maybe
import           Data.Text                    (Text)
import qualified Data.Text                    as Text

import           Text.Parser.Combinators      (choice, sepBy1, sepByNonEmpty)
import           Text.Read                    (readMaybe)
import           Data.Attoparsec.Text (parseOnly, Parser)
import           Text.Parser.Char (text, newline, char)

import Test.QuickCheck.Arbitrary (Arbitrary (arbitrary))
import Test.QuickCheck (property)

import Elves hiding (right, left)
import Elves.Advent (day)
import qualified Elves.Zipper as Z
import           Elves.Zipper (Zipper, left, right)

data PlantRule = PlantRule { l1, l0, c, r0, r1, ret :: Bool }
  deriving (Eq, Show)

instance Arbitrary PlantRule where
  arbitrary = PlantRule <$> arbitrary <*> arbitrary
                        <*> arbitrary
                        <*> arbitrary <*> arbitrary
                        <*> arbitrary

type Rule a b = Maybe a -> Maybe a -> Maybe a -> Maybe a -> Maybe a -> b

type PlantState = A.UArray Int Bool

main :: IO ()
main = day 12 inputP pt1 pt2 spec
  where
    pt1 = getFingerPrint 20
    pt2 = assumingCycle 50000000000
    -- assumes there is a cycle - this is needed to complete pt 2
    assumingCycle n (s, rules) = do
      let c = detectCycle s rules
          stepsRemaining = (n - cycleStart c) `div` cycleLength c
          nextStart = cycleStartVal c + cycleVal c * stepsRemaining
      case (n - cycleStart c) `rem` (cycleLength c) of
        0 -> print nextStart -- great, print and go
        r -> do -- we need to fast-fwd to the end of the last cycle we can use
                -- and then run the sim from there.
                let gen = runRulesA rules r (fromCycle r c)
                print (fingerprint $ fromPlantState gen)
    -- assumes there is no cycle
    getFingerPrint n (s,rules) = do
      let gen = runRulesA rules n (toPlantState s)
      print (fmap showPlant $ A.elems $ gen)
      print (fingerprint $ fromPlantState gen)

-- get the PlantState after n cycles, starting from where this cycle begins
fromCycle :: Int -> Cycle -> PlantState
fromCycle n c = let (Right z) = parseOnly (Z.fromNonEmpty . NE.fromList <$> some plantP)
                                          (cycleKey c)
                in toPlantState (z { Z.idx = cycleOffset c + (n * cycleOffsetDX c) })

data Cycle = Cycle
  { cycleKey      :: !Text
  , cycleStart    :: !Int
  , cycleLength   :: !Int
  , cycleStartVal :: !Int
  , cycleVal      :: !Int
  , cycleOffset   :: !Int
  , cycleOffsetDX :: !Int
  } deriving (Show)

-- pt 2 requires using the fact that the rules build cycles that can be
-- exploited to calculate ahead to the answer. This detects the first cycle
-- we encounter.
detectCycle :: Zipper Bool -> [PlantRule] -> Cycle
detectCycle s rules =
  let r = compileRules rules
      fp :: PlantState -> Int
      fp a = sum [i | (i, True) <- A.assocs a]
      key = Text.pack . fmap showPlant . A.elems
      init = toPlantState s
      go m n s = let s' = stepPlantState r s
                     k = key s'
                  in case HM.lookup k m of
                       Nothing -> go (HM.insert k (n, fp s', fst (A.bounds s')) m) (n + 1) s'
                       Just (step, f, os) -> Cycle k step (n - step) f (fp s' - f)
                                                   os (fst (A.bounds s') - os)
  in go (HM.singleton (key init) (0, fp init, fst (A.bounds init))) 1 init

inputP :: Parser (Zipper Bool, [PlantRule])
inputP = (,) <$> initialStateP <*> (newline >> newline >> sepBy1 ruleP newline)

initialStateP :: Parser (Zipper Bool)
initialStateP = text "initial state: " *> fmap (Z.fromNonEmpty . NE.fromList) (some plantP)

plantP :: Parser Bool
plantP = choice [True <$ char '#', False <$ char '.']

ruleP :: Parser PlantRule
ruleP = PlantRule <$> plantP
                  <*> plantP
                  <*> plantP
                  <*> plantP
                  <*> plantP
                  <*> (text " => " *> plantP)

compileRules :: [PlantRule] -> Rule Bool Bool
compileRules rs =
  let patterns = buildPatterns rs
   in \ma mb mc md me -> let pat = fromMaybe False <$> [ma, mb, mc, md, me]
                          in S.member pat patterns

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

stepPlantState :: Rule Bool Bool -> PlantState -> PlantState
stepPlantState r ps =
  let (i,j) = bs
      xs = [(i,val i) | i <- A.range (i - 3, j + 3)]
      trimmed = dropWhileEnd (not . snd) $ dropWhile (not . snd) xs
      is = fst <$> trimmed
  in A.array (minimum is, maximum is) trimmed
 where
   bs     = A.bounds ps
   a ?! i = pure (A.inRange bs i && (a :: PlantState) A.! i)
   val i = r (ps ?! (i - 2))
             (ps ?! (i - 1))
             (ps ?! i)
             (ps ?! (i + 1))
             (ps ?! (i + 2))


fromPlantState :: PlantState -> Zipper Bool
fromPlantState ps = let (Just z) = Z.fromList (A.elems ps)
                     in z { Z.idx = fst (A.bounds ps) }

toPlantState :: Zipper Bool -> PlantState
toPlantState z = let xs = Z.toList $ Z.indexed z
                     lb = minimum (fmap fst xs)
                     ub = maximum (fmap fst xs)
                  in A.array (lb,ub) xs

runRules :: [PlantRule] -> Zipper Bool -> [Zipper Bool]
runRules rules = let r = compileRules rules in iterate (stepState r)

buildPatterns :: [PlantRule] -> S.HashSet [Bool]
buildPatterns rs = S.fromList [ [l1,l0,c,r0,r1] | (PlantRule l1 l0 c r0 r1 True) <- rs ]

showState :: Zipper Bool -> Text
showState = Text.pack . fmap showPlant . Z.toList . Z.seekStart

showRule :: PlantRule -> String
showRule r = fmap showPlant inputs <> " => " <> [showPlant (ret r)]
  where inputs = [l1 r, l0 r, c r, r0 r, r1 r]

showPlant :: Bool -> Char
showPlant = bool '.' '#'

applyRule :: Rule a b -> Zipper a -> Zipper b
applyRule r = extend (rule r)

stepState :: Rule Bool Bool -> Zipper Bool -> Zipper Bool
stepState r = trim . applyRule r . Z.grow False

-- remove the useless Falses, introduced by growing or created
-- by rules eliminating ends.
trim :: Zipper Bool -> Zipper Bool
trim z = let ls' = dropWhileEnd not (Z.lefts z)
             rs' = dropWhileEnd not (Z.rights z)
          in z { Z.zlen = 1 + length ls' + length rs'
               , Z.lefts = ls'
               , Z.rights = rs'
               }

-- the zipper based rule evaluator.
rule :: Rule a b -> Zipper a -> b
rule r z = r (select (left >=> left))
             (select left)
             (select pure)
             (select right)
             (select (right >=> right))
  where select move = Z.focus <$> move z

fingerprint :: Zipper Bool -> Int
fingerprint z = sum [i | (i, True) <- Z.toList (Z.indexed z)]

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

spec :: Spec
spec = do
  let unzip = Z.toList . Z.seekStart . Z.indexed

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
    it "can parse and serialise correctly" $ property $ \z ->
      let expected = Z.rewind z
          serialized = "initial state: " <> showState z
       in parseOnly initialStateP serialized `shouldBe` Right expected

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
  -- TODO: move to zipper specs
  describe "indexed" $ do
    let f = unzip . Z.grow 'x'
    it "can index correctly, marking negative indices" $ do
      let z = Z.Zipper 0 1 [] '-' []
      f z `shouldBe` [(-2, 'x'), (-1, 'x')
                     ,(0, '-')
                     ,(1, 'x'), (2, 'x')
                     ]
    it "can index correctly, marking negative indices from different base" $ do
      let z = Z.Zipper 7 1 [] '-' []
      f z `shouldBe` [(5, 'x'), (6, 'x')
                     ,(7, '-')
                     ,(8, 'x'), (9, 'x')
                     ]

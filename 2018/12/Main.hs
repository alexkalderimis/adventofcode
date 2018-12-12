{-# LANGUAGE DeriveFunctor #-}

import Test.Hspec
import Test.QuickCheck (Arbitrary, arbitrary, property)
import qualified Data.HashSet as S
import qualified Data.List.NonEmpty as NE
import           Data.List.NonEmpty (NonEmpty((:|)))
import           Data.List (dropWhileEnd)
import Data.Maybe
import Control.Comonad
import Control.Monad
import Data.Bool
import System.Exit
import System.Environment
import Control.Applicative
import qualified Data.Text as Text
import           Data.Text (Text)
import qualified Data.HashMap.Strict as HM
import Text.Read (readMaybe)
import Text.Parser.Combinators (choice, sepBy1, sepByNonEmpty)
import Text.Parser.Char
import Text.ParserCombinators.ReadP (readP_to_S, ReadP)
import qualified Data.Array.Unboxed as A

data Zipper a = Zipper { ls :: [a], focus :: a, rs :: [a] }
  deriving (Eq, Show, Functor)

data PlantRule = PlantRule { l1, l0, c, r0, r1, ret :: Bool }
  deriving (Eq, Show)

instance Arbitrary PlantRule where
  arbitrary = PlantRule <$> arbitrary <*> arbitrary
                        <*> arbitrary
                        <*> arbitrary <*> arbitrary
                        <*> arbitrary

instance Arbitrary a => Arbitrary (Zipper a) where
  arbitrary = Zipper <$> arbitrary <*> arbitrary <*> arbitrary

type Rule a b = Maybe a -> Maybe a -> Maybe a -> Maybe a -> Maybe a -> b

type PlantState = A.UArray Int Bool

main :: IO ()
main = do
  inp <- getContents >>= maybe (die "Could not parse input") pure . runParser inputP
  args <- getArgs
  case args of
    []               -> getFingerPrint inp 20
    ["cycle"]        -> showCycle inp
    ["use-cycle", s] -> maybe (die $ "Could not read " ++ s ++ " as an int")
                              (assumingCycle inp)
                              (readMaybe s)
    [s] -> maybe (die $ "Could not read " ++ s ++ " as an int") (getFingerPrint inp)
                 (readMaybe s)
    _ -> die $ "bad arguments, expected a number"
  where
    showCycle inp = print (uncurry detectCycle inp)
    -- assumes there is a cycle - this is needed to complete pt 2
    assumingCycle (s, rules) n = do
      let (_, step0, cycleLength, val0, stepSize) = detectCycle s rules
          stepsRemaining = (n - step0) `div` cycleLength
      print (val0 + stepSize * stepsRemaining)
    -- assumes there is no cycle
    getFingerPrint (s,rules) n = do
      let gen = runRulesA rules n (toPlantState s)
      print (fmap showPlant $ A.elems $ gen)
      print (fingerprint $ fromPlantState gen)

-- pt 2 requires using the fact that the rules build cycles that can be
-- exploited to calculate ahead to the answer. This detects the first cycle
-- we encounter.
detectCycle :: Zipper Bool -> [PlantRule] -> (Text, Int, Int, Int, Int)
detectCycle s rules =
  let r = compileRules rules
      fp :: PlantState -> Int
      fp a = sum [i | (i, True) <- A.assocs a]
      key = Text.pack . fmap showPlant . A.elems
      init = toPlantState s
      go m n s = let s' = stepPlantState r s
                     k = key s'
                  in case HM.lookup k m of
                       Nothing -> go (HM.insert k (n, fp s') m) (n + 1) s'
                       Just (step, f) -> (k, step, n - step, f, fp s' - f)
  in go (HM.singleton (key init) (0, fp init)) 1 init

inputP :: ReadP (Zipper Bool, [PlantRule])
inputP = (,) <$> initialStateP <*> (newline >> newline >> sepBy1 ruleP newline)

initialStateP :: ReadP (Zipper Bool)
initialStateP = string "initial state: " *> fmap (fromNonEmpty . NE.fromList) (some plantP)

plantP :: ReadP Bool
plantP = choice [True <$ char '#', False <$ char '.']

ruleP :: ReadP PlantRule
ruleP = do
  l1 <- plantP
  l0 <- plantP
  c  <- plantP
  r0 <- plantP
  r1 <- plantP
  string " => "
  ret <- plantP
  return (PlantRule l1 l0 c r0 r1 ret)

runParser :: ReadP a -> String -> Maybe a
runParser p = fmap fst . listToMaybe . reverse . readP_to_S p

compileRules :: [PlantRule] -> Rule Bool Bool
compileRules rs =
  let patterns = buildPatterns rs
   in \ma mb mc md me -> let pat = fmap (fromMaybe False) $ [ma, mb, mc, md, me]
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
fromPlantState ps = let (Just z) = fromList (A.assocs ps)
                     in fmap snd $ recentre z

-- recentering is expensive for states that are very far away from
-- the origin, so could be eliminated by having zippers carry around
-- a notion of an offset. This was not necessary for this exercise
-- however.
recentre :: Zipper (Int, Bool) -> Zipper (Int, Bool)
recentre z = case compare (fst $ focus z) 0 of
  EQ -> z
  GT -> recentre $ fromMaybe (Zipper [] (pred $ fst $ focus z, False) (focus z : rs z))
                 $ left z
  LT -> recentre $ fromMaybe (Zipper (focus z : ls z) (succ $ fst $ focus z, False) [])
                 $ right z

toPlantState :: Zipper Bool -> PlantState
toPlantState z = let xs = toList $ indexed z
                     lb = minimum (fmap fst xs)
                     ub = maximum (fmap fst xs)
                  in A.array (lb,ub) xs

runRules :: [PlantRule] -> Zipper Bool -> [Zipper Bool]
runRules rules = let r = compileRules rules
                  in iterate (applyRule r . grow False)

buildPatterns :: [PlantRule] -> S.HashSet [Bool]
buildPatterns rs = S.fromList [ [l1,l0,c,r0,r1] | (PlantRule l1 l0 c r0 r1 True) <- rs ]

right :: Zipper a -> Maybe (Zipper a)
right (Zipper lhs old (new:rhs)) = Just $ Zipper (old:lhs) new rhs
right _ = Nothing

left :: Zipper a -> Maybe (Zipper a)
left (Zipper (new:lhs) old rhs) = Just $ Zipper lhs new (old:rhs)
left _ = Nothing

rewind :: Zipper a -> Zipper a
rewind z = case left z of
  Nothing -> z
  Just lz -> rewind lz

fromNonEmpty :: NonEmpty a -> Zipper a
fromNonEmpty (a :| as) = Zipper [] a as

fromList :: [a] -> Maybe (Zipper a)
fromList [] = Nothing
fromList (a:as) = Just $ Zipper [] a as

toList :: Zipper a -> [a]
toList z = reverse (ls z) ++ focus z : rs z

showState :: Zipper Bool -> String
showState = fmap showPlant . toList

showRule :: PlantRule -> String
showRule r = fmap showPlant inputs ++ " => " ++ [showPlant (ret r)]
  where inputs = [l1 r, l0 r, c r, r0 r, r1 r]

showPlant :: Bool -> Char
showPlant = bool '.' '#'

instance Applicative Zipper where
  pure a = Zipper [] a []
  zf <*> za = Zipper (ls zf <*> ls za) (focus zf $ focus za) (rs zf <*> rs za)

instance Comonad Zipper where
  extract = focus
  duplicate z = let shift dir = catMaybes . takeWhile isJust . tail . iterate (>>= dir) . Just
                 in Zipper (shift left z) z (shift right z)

applyRule :: Rule a b -> Zipper a -> Zipper b
applyRule r = extend (rule r)

stepState :: Rule Bool Bool -> Zipper Bool -> Zipper Bool
stepState r = trim . applyRule r . grow False

-- we could have infinite zippers, but growing them is
-- slightly better as they stay showable.
grow :: a -> Zipper a -> Zipper a
grow a (Zipper ls c rs) = Zipper (ls ++ replicate 3 a) c (rs ++ replicate 3 a)

-- remove the useless Falses, introduced by growing or created
-- by rules eliminating ends.
trim :: Zipper Bool -> Zipper Bool
trim (Zipper ls c rs) = Zipper (dropWhileEnd not ls) c (dropWhileEnd not rs)

indexed :: Zipper a -> Zipper (Int, a)
indexed (Zipper ls c rs) = Zipper (zip (iterate pred (-1)) ls) (0, c) (zip (iterate succ 1) rs)

-- the zipper based rule evaluator.
rule :: Rule a b -> Zipper a -> b
rule r z = r (select (left >=> left))
             (select left)
             (select pure)
             (select right)
             (select (right >=> right))
  where select move = focus <$> move z

fingerprint :: Zipper Bool -> Int
fingerprint z = sum [i | (i, True) <- toList (indexed z)]

exampleInput :: String
exampleInput = unlines
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
  describe "inputP" $ do
    let mInp = runParser inputP exampleInput
    it "has the correct number of rules" $ do
      fmap (length . snd) mInp `shouldBe` Just 14
    it "has the correct initial state" $ do
      let state = "#..#.#..##......###...###"
      fmap (showState . fst) mInp `shouldBe` Just state
    it "has rule 7 correct" $ do
      let rule = "#.#.# => #" -- rule 7.
      fmap (showRule . (!! 7) . snd) mInp `shouldBe` Just rule
  describe "State" $ do
    it "can parse and serialise correctly" $ property $ \z ->
      runParser initialStateP ("initial state: " ++ showState z) `shouldBe` Just (rewind z)
  describe "PlantRule" $ do
    it "can parse and serialise correctly" $ do
      property $ \r -> runParser ruleP (showRule r) `shouldBe` Just r
  describe "applyRule" $ do
    let (Just (state, rules)) = runParser inputP exampleInput
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
    let (Just (state, rules)) = runParser inputP exampleInput
    it "should step correctly from state r -> state 3, growing the state" $ do
      let s = runRules rules state !! 3
      showState (trim s) `shouldBe` "#.#...#..#.#....#..#..#...#"
    it "can proceed to state 20, as per the example" $ do
      let s = runRules rules state !! 20
          Just asExpected = runParser (some plantP) "#....##....#####...#######....#.#..##"
      toList (indexed $ trim s) `shouldBe` zip [-2 .. ] asExpected
  describe "potFingerprint" $ do
    it "gets the right fingerprint for generation 20" $ do
      let (Just (state, rules)) = runParser inputP exampleInput
          s = runRules rules state !! 20
      fingerprint s `shouldBe` 325
  describe "indexed" $ do
    it "can index correctly, marking negative indices" $ do
      let grown = toList . indexed . grow 'x' $ pure '-'
      grown `shouldBe` [(-4, 'x'), (-3, 'x'), (-2, 'x'), (-1, 'x')
                        ,(0, '-')
                        ,(1, 'x'), (2, 'x'), (3, 'x'), (4, 'x')
                        ]


{-# LANGUAGE BangPatterns #-}

module Elves (
  pairs,
  getMatrix,
  minimalBinarySearch,
  maximalBinarySearch,
  minmax,
  locally,
  boundedSearch,
  applyN, applyNWithCycleDetection, applyNM, applyNMWithCycleDetectionBy,
  cyclePred,
  cycleSucc,
  best, lowest, median,
  bestTree,
  count,
  namedExamples,
  testing,
  consider, which,
  interleave,
  unterleave,
  withNext,
  fmapToSnd,
  collapseForest,
  atMost, atLeast,
  on2,
  allPreds, anyPreds,
  (.*),
  (<#>),
  (<||>), (<&&>),
  module X
  ) where

import Control.Arrow as X
import Control.Monad.State.Class
import Control.Applicative as X
import Control.Monad as X
import Control.Monad.Reader
import Data.List.Extra as L
import Data.Maybe as X
import Data.Monoid as X
import Data.Ord as X
import Data.Tree (Forest,Tree(..))
import Data.Bool
import qualified Data.Text as Text
import qualified Data.Map.Strict     as M
import Data.Text (Text)
import Test.Hspec as X
import Test.Hspec.Core.Spec (SpecM)
import Data.Attoparsec.Text as X (Parser, parseOnly)

(<#>) :: Applicative f => f a -> f b -> f (a,b)
(<#>) = liftA2 (,)

infixl 3 <||>
(<||>) :: (a -> Bool) -> (a -> Bool) -> a -> Bool
f <||> g = (||) <$> f <*> g

infixl 3 <&&>
(<&&>) :: (a -> Bool) -> (a -> Bool) -> a -> Bool
f <&&> g = (&&) <$> f <*> g

pairs :: [a] -> [(a, a)]
pairs l = [(x,y) | (x:ys) <- tails l, y <- ys]

getMatrix :: (Read a) => IO [[a]]
getMatrix = fmap (fmap read . words) . lines <$> getContents

minmax :: Ord a => [a] -> Maybe (a,a)
minmax = foldl' (\mp x -> fmap (cmp x) mp <|> Just (x,x)) Nothing
  where cmp x (a,b) = (min x a, max x b)

-- call a function N times on a value
applyN :: Int -> (a -> a) -> a -> a
applyN n f = foldl' (.) id (replicate n f)

-- like applyN, but detects cycles.
applyNWithCycleDetection :: Ord a => Int -> (a -> a) -> a -> a
applyNWithCycleDetection n f x = go (M.singleton x n) n x
  where
    go !m !n !x 
      | n == 0     = x
      | otherwise  =
                 let x'   = f x
                     loop = maybe 0 (loopSize n) (M.lookup x' m)
                     n'   = n - (1 + loop)
                 in go (M.insert x' n' m) n' x'

    loopSize n loopStart = let loopLen = loopStart - (n - 1)
                               loops = (n - 1) `div` loopLen
                            in loops * loopLen

applyNM :: Monad m => Int -> (a -> m a) -> a -> m a
applyNM n act a = foldl' (>>=) (pure a) (replicate n act)

-- like applyNM, but detects cycles.
applyNMWithCycleDetectionBy :: (Ord b, Monad m) => (a -> b) -> Int -> (a -> m a) -> a -> m a
applyNMWithCycleDetectionBy k n f x = go (M.singleton (k x) n) n x
  where
    go !m !n !x 
      | n == 0     = pure x
      | otherwise  = do x' <- f x
                        let key = k x'
                            loop = maybe 0 (loopSize n) (M.lookup key m)
                            n'   = n - (1 + loop)
                            m'   = M.insert key n' m
                        go m' n' x'

    loopSize n loopStart = let loopLen = loopStart - (n - 1)
                               loops = (n - 1) `div` loopLen
                            in loops * loopLen

cyclePred :: (Enum a, Eq a, Bounded a) => a -> a
cyclePred x = if x == minBound then maxBound
                               else pred x

cycleSucc :: (Enum a, Eq a, Bounded a) => a -> a
cycleSucc x = if x == maxBound then minBound
                               else succ x

best, lowest :: Ord b => (a -> b) -> [a] -> Maybe a
best f = listToMaybe . sortOn (Down . f)
lowest f = listToMaybe . sortOn f

median :: Ord a => [a] -> Maybe a
median xs = let xs' = sort xs
             in listToMaybe $ drop (length xs `div` 2) xs'

bestTree :: (Monoid b, Ord b) => (a -> b) -> Forest a -> b
bestTree f = maximum . (mempty :) . fmap ((<>) <$> f . rootLabel <*> bestTree f . subForest)

count :: (a -> Bool) -> [a] -> Int
count f = length . filter f

collapseForest :: Monoid a => (Forest a -> a) -> Tree a -> a
collapseForest f (Node a fs) = a <> f fs

-- like local in Reader - this allows a stateful
-- action to run, and then have its modifications
-- discarded.
locally :: MonadState s m => m a -> m a
locally ma = do
  s <- get
  r <- ma
  put s
  return r

boundedSearch :: (Integral a, Num a, Enum a) => (a -> Bool) -> (a -> Bool) -> (a -> Bool) -> (a,a) -> Maybe a
boundedSearch lteq gteq eq bs = listToMaybe (filter eq [lb .. ub])
  where
    lb = maximalBinarySearch lteq bs
    ub = minimalBinarySearch gteq (lb, snd bs)

-- search downwards within the given range for our item, using bisection
minimalBinarySearch :: (Integral a, Num a) => (a -> Bool) -> (a,a) -> a
minimalBinarySearch isOK
  = binarySearch (subtract . bool 0 1 . even) $ \p rng ->
    if isOK p then (fst rng, p) else (p + 1, snd rng)

-- search upwards within the given range for our item, using bisection
maximalBinarySearch :: (Integral a, Num a) => (a -> Bool) -> (a,a) -> a
maximalBinarySearch isOK
  = binarySearch (pure id) $ \p rng ->
    if isOK p then (p, snd rng) else (fst rng, p - 1)

binarySearch :: (Integral a, Num a) => (a -> a -> a) -> (a -> (a,a) -> (a,a)) -> (a,a) -> a
binarySearch rangeAdjust nextRange = go
  where
    go rng | rngSize rng < 2 = fst rng
           | otherwise       = let p = mid rng in go (nextRange p rng)
    rngSize (a,b) = (b - a) + 1
    mid rng = let n = rngSize rng in fst rng + (rangeAdjust n $ div n 2)

namedExamples :: (Show a, Eq a, Functor f) => f (Text,a) -> f (String,Text,a)
namedExamples = fmap $ \(inp, a) -> (Text.unpack inp, inp, a)

testing :: (Show a, Eq a, Foldable t) => String -> Parser a -> t (String, Text, a) -> SpecWith ()
testing pref p examples = forM_ examples $ \(name, inp, ret) -> it (unwords [pref, name]) (parseOnly p inp `shouldBe` Right ret)

consider :: (Show a) => a -> ReaderT a (SpecM b) () -> SpecWith b
consider topic m = describe (show topic) (runReaderT m topic)

which :: (HasCallStack, Example a)
      => String -> (topic -> a) -> ReaderT topic (SpecM (Arg a)) ()
which msg test = do
  a <- ask
  lift (specify msg (test a))

interleave :: [a] -> [a] -> [a]
interleave [] ys = ys
interleave xs [] = xs
interleave (x:xs) (y:ys) = x : y : interleave xs ys

-- instead of interleaving two lists, it un-interleaves it, so that "abcde" becomes: ("ace", "bd")
unterleave :: [a] -> ([a], [a])
unterleave = go ([],[])
  where
    go (xs,ys) (a:b:cs) = go (a:xs,b:ys) cs
    go (xs,ys) cs       = (reverse xs <> cs, reverse ys)

withNext :: [a] -> [(a, Maybe a)]
withNext xs = zip xs (fmap pure (drop 1 xs) <> [Nothing])

fmapToSnd :: Functor f => (a -> b) -> f a -> f (a, b)
fmapToSnd f = fmap $ \a -> (a, f a)

-- I constantly confuse myself when using min/max to enforce
-- bounds - this helps.
atMost :: Ord a => a -> a -> a
atMost = min

atLeast :: Ord a => a -> a -> a
atLeast = max

-- transform arguments before applying to a binary function
on2 :: (a -> b -> c) -> (d -> a) -> (e -> b) -> d -> e -> c
on2 f da eb d e = f (da d) (eb e)

-- from Control.Composition
-- eg: flooring addition => atLeast 0 .* (+)
(.*) :: (c -> d) -> (a -> b -> c) -> a -> b -> d
(f .* g) a b = f (g a b)

allPreds :: [a -> Bool] -> a -> Bool
allPreds = (getAll .) . mconcat . fmap (All .)

anyPreds :: [a -> Bool] -> a -> Bool
anyPreds = (getAny .) . mconcat . fmap (Any .)

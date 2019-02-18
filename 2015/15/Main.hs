{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}

import           Control.Applicative.Combinators
import           Data.Attoparsec.Text            (decimal, signed)
import qualified Data.Attoparsec.Text            as A
import qualified Data.HashMap.Strict             as Map
import qualified Data.List                       as L
import           Data.Maybe
import           Data.Monoid
import           Data.Ord
import qualified Data.Set                        as S
import           Data.Text                       (Text)
import qualified Data.Text                       as T
import           Data.Tree                       (Tree (..))
import           Text.Parser.Char                (newline)

import           Test.QuickCheck

import           Elves
import           Elves.Advent
import           Elves.Geometry                  (Point)
import           Elves.Math.Expression           (Expr, var)

type Map = Map.HashMap

type Constraint a = Point a -> Bool

data Constrained b = Constrained (forall a . Num a => Ingredient a -> a) b

instance Functor Constrained where
  fmap f (Constrained fld v) = Constrained fld (f v)

-- an Ingredient is a Vector in the mathematical sense: it can be scaled
-- (in terms of quantities) and added together.
data Ingredient a = Ingredient
  { capacity   :: a -- (how well it helps the cookie absorb milk)
  , durability :: a -- (how well it keeps the cookie intact when full of milk)
  , flavor     :: a -- (how tasty it makes the cookie)
  , texture    :: a -- (how it improves the feel of the cookie)
  , calories   :: a -- (how many calories it adds to the cookie)
  , quantity   :: a -- (how many teaspoons we have applied so far)
  } deriving (Show, Eq, Functor)

useIngredient :: Num a => a -> Ingredient a -> Ingredient a
useIngredient n i = (fmap (n *) i) { quantity = n }

instance (Num a) => Semigroup (Ingredient a) where
  a <> b = let add f = on2 (+) f f
            in Ingredient (add capacity a b)
                          (add durability a b)
                          (add flavor a b)
                          (add texture a b)
                          (add calories a b)
                          (add quantity a b)

instance (Num a) => Monoid (Ingredient a) where
  mempty = Ingredient 0 0 0 0 0 0
  mappend = (<>)

type Teaspoons = Int
type Recipe a = Map Text (Ingredient a)

main :: IO ()
main = day 15 parser pt1 pt2 test
  where
    score = print . cookieScore . bakeCookie
    pt1 = score . bestRecipe 100 []
    pt2 = score . bestRecipe 100 [Constrained calories 500]

setQuantity :: Num a => Text -> a -> Recipe a -> Recipe a
setQuantity k n = Map.adjust (useIngredient n) k

bakeCookie :: Num a => Recipe a -> Ingredient a
bakeCookie = foldMap id

cookieScore :: (Ord a, Num a) => Ingredient a -> a
cookieScore x = product $ fmap (atLeast 0 . ($ x))
                               [capacity, durability, flavor, texture]

-- turn the ingredients into an objective function
measure :: (Num a, Ord a) => [Ingredient a] -> [a] -> a
measure is vs = cookieScore . mconcat $ appliedIs
  where
   appliedIs = zipWith useIngredient vs is

-- This is a genuinely fascinating problem, which admits both this solution, which
-- relies for efficiency on generating the search plane efficiently, without
-- resorting to filters or nubbing, but also more interestingly, it can be solved
-- symbolically using partial derivatives and Lagrangian multipliers. It also
-- has some truly beautiful graphs.
--
-- this is a problem in linear optimization, specifically it requires
-- us to maximise the value of the cookieScore under the constraint
-- sum quantities === 100.
--
-- In the case of the example below, this can be represented with the constraint
--
-- x + y == 100
--
-- and the objective
--
-- max(0, -1x+2y) * max(0, -2x+3y) * max(0, 6x-2y) * max(0, 3x-y) = q
--
-- Specifically, the Butterscotch and Cinnamon problem below can be respresented
-- with this graph: https://www.desmos.com/calculator/davvpx6roe
-- (see butterscotch-cinnamon.png in this directory)
--
-- Unfortunately the discontinuities in the function prevent us from using
-- the Lagrangian method directly, so we use a two stage solution
bestRecipe :: Teaspoons -> [Constrained Int] -> Recipe Int -> Recipe Int
bestRecipe n cs r = Map.fromList [(name, useIngredient n i) | (n, (name, i)) <- zip vs namedIs]
  where
    namedIs = Map.toList r
    is = snd <$> namedIs
    -- search the constrained plane
    vs = L.maximumBy (comparing (measure is))
       . filter (test is cs)
       $ planePoints n (L.genericLength is)

    test is cs vs = all (\c -> predicate is c vs) cs
    predicate is c vs =
      let (Constrained f goal) = c
       in goal == f (mconcat $ zipWith useIngredient vs is)

    -- inside = and . zipWith (\[lb,ub] p -> lb <= p && p <= ub) (L.transpose reg)

parser :: Parser (Recipe Int)
parser = fmap Map.fromList (ingredient `sepBy1` newline)
  where
    ingredient = (,) <$> (A.takeWhile (/= ':') <* ":")
                     <*> (Ingredient
                            <$> (" capacity " *> signed decimal)
                            <*> (", durability " *> signed decimal)
                            <*> (", flavor " *> signed decimal)
                            <*> (", texture " *> signed decimal)
                            <*> (", calories " *> signed decimal)
                            <*> pure 0)


-- generate just the points in the plane, without even
-- considering any other irrelevant points.
planePoints :: Int -> Word -> [Point Int]
planePoints total = untree . summingTree total
  where
    summingTree t 0 = []
    summingTree t 1 = [Node t []]
    summingTree t n = fmap (\x -> Node x $ summingTree (t - x) (n - 1)) [0 .. t]
    untree [] = [[]]
    untree ts = ts >>= \(Node x rst) -> fmap (x:) (untree rst)

test = do
  let rec = parseOnly parser exampleInput
      optimum = 62842880
  describe "planePoints" $ do
    -- purposely small-ish inputs
    let inputs      = choose (1, 50) <#> choose (1, 5)
        smallInputs = choose (1, 20) <#> choose (1, 4)
    let totalSpace t n = (t + 1) ^ fromIntegral n
    specify "all points sum to the given value" . property
      . forAll inputs $ \(t,n) -> all ((== t) . sum) (planePoints t n)
    it "includes fewer points than the entire search space" . property
      . forAll inputs $ \(t,n) -> length (planePoints t n) < totalSpace t n
    -- because this test involves traversing the entire seach space, we use
    -- the smaller inputs for speed. Change to 'inputs' for a more
    -- thorough test
    it "includes all valid points" . property . forAll smallInputs $ \(t,n) ->
        let ps = S.fromList $ planePoints t n
            xs = [ 0 .. t ]
            allPoints = foldr (<*>) (fmap pure xs)
                              (replicate (fromIntegral n - 1) (fmap (:) xs))
         in not $ any (\p -> sum p == t && not (S.member p ps)) allPoints

  describe "example" $ do
    it "calculates the correct total" $ do
      let eg = cookieScore  . bakeCookie . setQuantity "Butterscotch" 44
                                         . setQuantity "Cinnamon" 56
      fmap eg rec `shouldBe` Right optimum

  describe "bestRecipe" $ do
    it "can find the optimum recipe" $ do
      let bake = cookieScore . bakeCookie . bestRecipe 100 []
      fmap bake rec `shouldBe` Right optimum
    it "can find the optimum calorie constrained recipe" $ do
      let bake = cookieScore . bakeCookie
                             . bestRecipe 100 [Constrained calories 500]
      fmap bake rec `shouldBe` Right 57600000
    it "gets the calorie constrained recipe right" $ do
      let f = setQuantity "Butterscotch" 40 . setQuantity "Cinnamon" 60
      fmap (bestRecipe 100 [Constrained calories 500]) rec `shouldBe` fmap f rec
    it "sets the recipe correctly" $ do
      let f = setQuantity "Butterscotch" 44 . setQuantity "Cinnamon" 56
      fmap (bestRecipe 100 []) rec `shouldBe` fmap f rec
    specify "the recipe contains exactly n teaspoons" . property
      $ \(Positive n) ->
        fmap (sum . fmap quantity . Map.elems . bestRecipe n []) rec === Right n

exampleInput = T.unlines $ fmap T.unwords
  [["Butterscotch: capacity -1, durability -2,"
                 ,"flavor 6, texture 3, calories 8"
   ]
  ,["Cinnamon: capacity 2, durability 3, flavor -2,"
             ,"texture -1, calories 3"
   ]
  ]

-- the algebraic expression that the butterscotch/cinnamon recipe represents
exampleExpr :: Expr Double
exampleExpr = let x = var "Butterscotch"
                  y = var "Cinnamon"
               in product [2 * y - x, 3 * y - 2 * x, 6 * x - 2 * y, 3 * x - y]


{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE RankNTypes     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}

import           Control.Applicative.Combinators
import           Data.Attoparsec.Text            (decimal, signed)
import qualified Data.Attoparsec.Text            as A
import qualified Data.HashMap.Strict             as Map
import qualified Data.List                       as L
import           Data.Maybe
import           Data.Monoid
import           Data.Ord
import           Data.Sequence                   (Seq, ViewL (..))
import qualified Data.Sequence                   as Seq
import           Data.Text                       (Text)
import qualified Data.Text                       as T
import           Text.Parser.Char                (newline)

import           Test.QuickCheck

import           Elves
import           Elves.Advent

type Map = Map.HashMap

-- nicer names for these things
type Point a = [a]
type Region a = [Point a]

type Constraint a = Point a -> Bool

data Constrained b = Constrained (forall a. Ingredient a -> a) b

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
  } deriving (Show, Eq, Functor)

useIngredient :: Num a => a -> Ingredient a -> Ingredient a
useIngredient n = fmap (n *)

instance (Num a) => Semigroup (Ingredient a) where
  a <> b = let add f = on2 (+) f f
            in Ingredient (add capacity a b)
                          (add durability a b)
                          (add flavor a b)
                          (add texture a b)
                          (add calories a b)

instance (Num a) => Monoid (Ingredient a) where
  mempty = Ingredient 0 0 0 0 0
  mappend = (<>)

type Teaspoons = Int
type Recipe a = Map Text (a, Ingredient a)

main :: IO ()
main = day 15 parser pt1 pt2 test
  where
    pt1 = print . cookieScore . bakeCookie . bestRecipe 100 []
    pt2 = print

setQuantity :: Text -> a -> Recipe a -> Recipe a
setQuantity k n = Map.adjust (\(_, i) -> (n,i)) k

bakeCookie :: Num a => Recipe a -> Ingredient a
bakeCookie = foldMap (uncurry useIngredient)

cookieScore :: (Ord a, Num a) => Ingredient a -> a
cookieScore x = product $ fmap (atLeast 0 . ($ x))
                               [capacity, durability, flavor, texture]

-- Bunch of functions for dealing with N-dimensional geometry:

midpoint :: [Double] -> [Double] -> [Double]
midpoint = zipWith (\a b -> (a + b) / 2)

regionCentre :: Fractional a => Region a -> Point a
regionCentre es = let n = fromIntegral $ length es
                   in [sum p / n | p <- L.transpose es]

-- expand the region by a new dimension
extrema :: Num a => a -> Region a -> Region a
extrema n [] = [[n]]
extrema n es = (n : fmap (pure 0) es) : fmap (0 :) es

-- Split a region (defined by extrema) into a set of sub-regions
split :: Eq a => Region a -> Point a -> [Region a]
split reg p = fmap (\e -> p : filter (/= e) reg) reg

-- move this point one step closer to the goal
move :: (Ord objective, Num a)
     => (Point a -> objective) -> Constraint a -> a -> Point a -> Point a
move obj constraint eta p = L.maximumBy (comparing obj)
                          . filter constraint
                          $ expand eta p

-- return this point, expanded by +/- eta in all dimensions, as well
-- as the point itself
expand :: Num a => a -> Point a -> [Point a]
expand _ []       = [[]]
expand eta (p:ps) = (:) <$> [p, p - eta, p + eta] <*> expand eta ps

distance :: Floating c => Point c -> Point c -> c
distance a b = sqrt . product $ zipWith (*) a b

-- Now the interesting bit: this problem has two states - either
-- it is zero, and unfeasible or it is positive and convex and we can
-- follow the gradiant
--
-- So we have two functions, one for looking for any feasible
-- point within the region, and the other for following the
-- gradiant

-- generate an infinite stream of points that are converging
-- to the objective, and then pick the best one
followGradiant :: (Ord objective, Floating var, Ord var) =>
     (Point var -> objective) -> Constraint var -> var -> var -> Point var -> Point var
followGradiant obj constraint eta theta p
  = converge $ iterate (move obj constraint eta) p
  where
    converge [] = error "no more values - impossible!"
    converge [x] = x -- also impossible, but fine, return
    converge (a:b:xs) | a == b = a
    converge (a:b:xs) | obj a > obj b = a
    converge (a:b:xs) = if distance a b > theta
                           then converge (b:xs)
                           else b

-- if this isn't a feasible point, use a breadth-first-search
-- to try and find at least one feasible point
-- We stop splitting when the split is too small
findFeasible :: (Ord objective, Num objective, Ord var, Floating var) =>
  (Point var -> objective) -> Region var -> Point var -> Maybe (Point var)
findFeasible obj reg p
  | feasible p = Just p
  | otherwise = go (Seq.singleton (reg, regionCentre reg))
  where
    feasible x = obj x > 0
    cutoff = 1
    go q = case Seq.viewl q of
      EmptyL -> Nothing
      (reg,ctr) :< queue ->
        let regs = split reg ctr
            centres = filter ((> cutoff) . distance ctr)
                      $ fmap regionCentre regs
            recur = go (queue <> Seq.fromList (zip regs centres))
        in L.find feasible (ctr:centres) <|> recur

-- first find a feasible point, then follow the gradiant to the
-- best point. If this dimension is not solvable, it will return
-- an unfeasible point.
solveDimension :: (Ord a, Floating a) =>
   Constraint a -> [Ingredient a] -> Region a -> Point a -> Point a
solveDimension g is es p =
  let obj = measure is
      feasible = fromMaybe p $ findFeasible obj es p
   in followGradiant obj g 1 0.5 feasible

-- turn the ingredients into an objective function
measure :: (Num a, Ord a) => [Ingredient a] -> [a] -> a
measure is vs = cookieScore . mconcat $ zipWith useIngredient vs is

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
bestRecipe n cs r = Map.fromList [(name, (n, i)) | (n, (name, i)) <- zip vs namedIs]
  where
    namedIs = fmap snd <$> Map.toList r
    reg = head . dropWhile ((< Map.size r) . length) $ iterate (extrema (dbl n)) []
    -- run the solver on the point, mapping back to integral coordindates
    vs = let is = fmap (fmap dbl . snd) namedIs
             g = constraint is (dbl n) (fmap dbl <$> cs)
          in integralPoint $ solveDimension g is reg (head reg)

    -- take a continuous point and return an integral one
    integralPoint = let is = fmap snd namedIs
                     in L.maximumBy (comparing (measure is))
                        . filter (constraint is n cs)
                        . foldr (\x -> ([(floor x :), (ceiling x :)] <*>)) (pure [])

    dbl :: Int -> Double
    dbl = fromIntegral

constraint :: (Eq a, Num a) => [Ingredient a] -> a -> [Constrained a] -> Constraint a
constraint is n = L.foldl' compileConstraint ((== n) . sum)
  where
    compileConstraint f (Constrained fld v) pnt =
      let val = sum . fmap fld $ zipWith useIngredient pnt is
       in f pnt && (v == val)

parser :: Parser (Recipe Int)
parser = fmap Map.fromList (ingredient `sepBy1` newline)
  where
    ingredient = (,) <$> (A.takeWhile (/= ':') <* ":")
                     <*> fmap (0,) (Ingredient
                            <$> (" capacity " *> signed decimal)
                            <*> (", durability " *> signed decimal)
                            <*> (", flavor " *> signed decimal)
                            <*> (", texture " *> signed decimal)
                            <*> (", calories " *> signed decimal))

test = do
  let rec = parseOnly parser exampleInput
      optimum = 62842880
  describe "example" $ do
    it "calculates the correct total" $ do
      let eg = cookieScore  . bakeCookie . setQuantity "Butterscotch" 44
                                         . setQuantity "Cinnamon" 56
      fmap eg rec `shouldBe` Right optimum

  describe "bestRecipe" $ do
    it "can find the optimum recipe" $ do
      let bake = cookieScore . bakeCookie . bestRecipe 100 []
      fmap bake rec `shouldBe` Right optimum
    it "sets the recipe correctly" $ do
      let f = setQuantity "Butterscotch" 44 . setQuantity "Cinnamon" 56
      fmap (bestRecipe 100 []) rec `shouldBe` fmap f rec
    specify "the recipe contains exactly n teaspoons" . property
      $ \(Positive n) ->
        fmap (sum . fmap fst . Map.elems . bestRecipe n []) rec === Right n

exampleInput = T.unlines $ fmap T.unwords
  [["Butterscotch: capacity -1, durability -2,"
                 ,"flavor 6, texture 3, calories 8"
   ]
  ,["Cinnamon: capacity 2, durability 3, flavor -2,"
             ,"texture -1, calories 3"
   ]
  ]


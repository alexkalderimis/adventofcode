{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}

import           Control.Applicative.Combinators
import           Data.Attoparsec.Text            (decimal, signed)
import qualified Data.Attoparsec.Text            as A
import qualified Data.HashMap.Strict             as Map
import qualified Data.List                       as L
import           Data.Text                       (Text)
import qualified Data.Text                       as T
import           Text.Parser.Char                (newline)

import           Elves
import           Elves.Advent

type Map = Map.HashMap

data Ingredient = Ingredient
  { capacity   :: Int -- (how well it helps the cookie absorb milk)
  , durability :: Int -- (how well it keeps the cookie intact when full of milk)
  , flavor     :: Int -- (how tasty it makes the cookie)
  , texture    :: Int -- (how it improves the feel of the cookie)
  , calories   :: Int -- (how many calories it adds to the cookie)
  } deriving (Show, Eq)

instance Semigroup Ingredient where
  a <> b = let add f = on2 (+) f f
            in Ingredient (add capacity a b)
                          (add durability a b)
                          (add flavor a b)
                          (add texture a b)
                          (add calories a b)

instance Monoid Ingredient where
  mempty = Ingredient 0 0 0 0 0
  mappend = (<>)

type Teaspoons = Int
type Recipe = Map Text (Teaspoons, Ingredient)

main :: IO ()
main = day 15 parser pt1 pt2 test
  where
    pt1 = print . cookieScore . bakeCookie . bestRecipe 100
    pt2 _ = error "unimplemented"

setQuantity :: Text -> Teaspoons -> Recipe -> Recipe
setQuantity k n = Map.adjust (\(_, i) -> (n,i)) k

bakeCookie :: Recipe -> Ingredient
bakeCookie = foldMap (\(n,i) -> mconcat (replicate n i))

cookieScore :: Ingredient -> Int
cookieScore x = product $ fmap (atLeast 0 . ($ x))
                               [capacity, durability, flavor, texture]

bestRecipe :: Teaspoons -> Recipe -> Recipe
bestRecipe _ = id

parser :: Parser Recipe
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
      let eg = cookieScore . bakeCookie . setQuantity "Butterscotch" 44
                                        . setQuantity "Cinnamon" 56
      fmap eg rec `shouldBe` Right optimum

  describe "bestRecipe" $ do
    it "can find the optimum recipe" $ do
      let bake = cookieScore . bakeCookie . bestRecipe 100
      fmap bake rec `shouldBe` Right optimum


exampleInput = T.unlines $ fmap T.unwords
  [["Butterscotch: capacity -1, durability -2,"
                 ,"flavor 6, texture 3, calories 8"
   ]
  ,["Cinnamon: capacity 2, durability 3, flavor -2,"
             ,"texture -1, calories 3"
   ]
  ]


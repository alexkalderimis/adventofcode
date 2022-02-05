{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE NumericUnderscores #-}

import           Control.Applicative
import qualified Data.Array          as A
import           Data.Array          ((//))
import           Data.List.Extra
import           Data.Functor
import           Text.Printf
import           Control.Applicative.Combinators
import           Text.Parser.Char (text)
import qualified Data.Text as T

import Elves
import Elves.Advent
import qualified Elves.StrictGrid as G
import           Elves.Coord.Strict

import Test.QuickCheck

data Item = OpenGround | Trees | Lumberyard deriving (Show, Eq, Ord)
data Surroundings = Surroundings { trees :: {-# UNPACK #-} !Int, lumberyards :: {-# UNPACK #-} !Int }
  deriving (Show, Eq)

type Coord = Coordinate

type LandScape = A.Array Coord Item

instance Semigroup Surroundings where
  (Surroundings ta la) <> (Surroundings tb lb) = Surroundings (ta + tb) (la + lb)

instance Monoid Surroundings where
  mempty = Surroundings 0 0

nextItem :: Item -> Item
nextItem OpenGround = Trees
nextItem Trees = Lumberyard
nextItem Lumberyard = OpenGround

toSurroundings :: Item -> Surroundings
toSurroundings OpenGround = mempty
toSurroundings Trees      = Surroundings 1 0
toSurroundings Lumberyard = Surroundings 0 1

valueOf :: Surroundings -> Int
valueOf (Surroundings a b) = a * b

exampleInput = T.unlines
  [".#.#...|#."
  ,".....#|##|"
  ,".|..|...#."
  ,"..|#.....#"
  ,"#.#|||#|#|"
  ,"...#.||..."
  ,".|....|..."
  ,"||...#|.#|"
  ,"|.||||..|."
  ,"...#.|..|."
  ]

main :: IO ()
main = day 18 parseInput
              (print . valueOf . resourceValue . applyN 10 evolve)
              (print . valueOf . resourceValue . applyNWithCycleDetection 1_000_000_000 evolve)
              spec

spec = do
  let Right land = parseOnly parseInput exampleInput
  describe "evolve" $ do
    let afterOneMinute = T.unlines [ ".......##."
                                   , "......|###"
                                   , ".|..|...#."
                                   , "..|#||...#"
                                   , "..##||.|#|"
                                   , "...#||||.."
                                   , "||...|||.."
                                   , "|||||.||.|"
                                   , "||||||||||"
                                   , "....||..|."
                                   ]
    it "can evolve the initial state" $ do
      let Right land' = parseOnly parseInput afterOneMinute
      evolve land `shouldBe` land'

  describe "pt1" $ do
    let outcome = applyN 10 evolve land
    it "has 37 wooded acres and 31 lumberyards" $ do
      resourceValue outcome `shouldBe` Surroundings 37 31

  describe "surroundings" $ do
    it "produces realistic counts" $ 
      forAll (chooseInt (0, 5)) $ \y ->
      forAll (chooseInt (0, 5)) $ \x ->
      forAll (vectorOf 25 (elements [Trees, Lumberyard, OpenGround])) $ \items ->
        let land = A.listArray (Coord 0 0, Coord 5 5) (cycle items)
            Surroundings t ly = surroundings land (Coord (Row y) (Col x))
         in (t + ly) `shouldSatisfy` A.inRange (0, 8)

  describe "transform" $ do
    describe "an open acre" $ do
      let acre = OpenGround

      specify "surrounded by three or more trees becomes trees" $
        forAll (arbitrary `suchThat` (>= 3)) $ \t ->
        forAll arbitrary $ \ly ->
          let s = Surroundings { trees = t, lumberyards = ly }
           in transform s acre `shouldBe` Just Trees
      specify "surrounded by fewer than three trees does not change" $
        forAll (arbitrary `suchThat` (< 3)) $ \t ->
        forAll arbitrary $ \ly ->
          let s = Surroundings { trees = t, lumberyards = ly }
           in transform s acre `shouldBe` Nothing

    describe "an acre filled with trees" $ do
      let acre = Trees

      specify "will become a lumberyard if three or more adjacent acres were lumberyards." $
        forAll (arbitrary `suchThat` (>= 3)) $ \ly ->
        forAll arbitrary $ \t ->
          let s = Surroundings { trees = t, lumberyards = ly }
           in transform s acre `shouldBe` Just Lumberyard

      specify "Otherwise, nothing happens." $
        forAll (arbitrary `suchThat` (< 3)) $ \ly ->
        forAll arbitrary $ \t ->
          let s = Surroundings { trees = t, lumberyards = ly }
           in transform s acre `shouldBe` Nothing

    describe "An acre containing a lumberyard" $ do
      let acre = Lumberyard

      it "will remain a lumberyard if it was adjacent to at least one other lumberyard and at least one acre containing trees." $
        forAll (arbitrary `suchThat` (>= 1)) $ \t ->
        forAll (arbitrary `suchThat` (>= 1)) $ \ly ->
          let s = Surroundings { trees = t, lumberyards = ly }
           in transform s acre `shouldBe` Nothing

      it "becomes open if not next to at least one lumberyard." $ 
        forAll (arbitrary `suchThat` (>= 1)) $ \t ->
        forAll (arbitrary `suchThat` (<  1)) $ \ly ->
          let s = Surroundings { trees = t, lumberyards = ly }
           in transform s acre `shouldBe` Just OpenGround

      it "becomes open if not next to at least one acre of trees." $
        forAll (arbitrary `suchThat` (<  1)) $ \t ->
        forAll (arbitrary `suchThat` (>= 1)) $ \ly ->
          let s = Surroundings { trees = t, lumberyards = ly }
           in transform s acre `shouldBe` Just OpenGround

resourceValue :: LandScape -> Surroundings
resourceValue = foldMap toSurroundings . A.elems

showLand :: LandScape -> String
showLand = G.draw . fmap cell
  where
    cell OpenGround = '.'
    cell Trees      = '|'
    cell Lumberyard = '#'

parseInput :: Parser LandScape
parseInput = G.gridP $ choice [ text "." $> OpenGround
                              , text "|" $> Trees
                              , text "#" $> Lumberyard
                              ]

evolve :: LandScape -> LandScape
evolve items = items // (A.assocs items >>= tick)
  where
    tick (pos, x) = [ (pos, x') | x' <- transform (surroundings items pos) x ]

{-# INLINE surroundings #-}
surroundings :: LandScape -> Coord -> Surroundings
surroundings items pos = foldMap (toSurroundings . (items A.!)) (G.nextCoords True (A.bounds items) pos)

{-# INLINE transform #-}
transform :: (MonadPlus m) => Surroundings -> Item -> m Item
transform s item
  | OpenGround <- item, trees s       >= 3               = pure Trees
  | Trees      <- item, lumberyards s >= 3               = pure Lumberyard
  | Lumberyard <- item, lumberyards s < 1 || trees s < 1 = pure OpenGround
  | otherwise = mzero

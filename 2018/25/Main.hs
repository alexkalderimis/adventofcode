{-# LANGUAGE OverloadedStrings          #-}

import           Control.Lens
import           Data.Attoparsec.Text    (decimal, signed)
import           Data.Ix                 (Ix)
import qualified Data.List               as L
import qualified Data.Set                as S
import qualified Data.Text               as Text
import           Text.Parser.Char        (newline)
import           Text.Parser.Combinators (sepEndBy1)

import           Elves
import           Elves.Advent
import           Elves.Clique
import           Elves.RTree             (Coord, RTree)
import qualified Elves.RTree             as RT

import Test.QuickCheck

type Point = (Int,Int,Int,Int)

main :: IO ()
main = day 25 inputP pt1 pt2 test
  where
    pt1 = print . numberOfConstellations
    pt2 _ = putStrLn "woot!"

coordP :: Parser Point
coordP = (,,,) <$> (int <* ",") <*> (int <* ",") <*> (int <* ",") <*> int
  where int = signed decimal

inputP :: Parser [Point]
inputP = coordP `sepEndBy1` newline

manhattan :: Coord i => i -> i -> Int
manhattan a b = sum [abs (view (runLens l) a - view (runLens l) b) | l <- RT.dimensions]

pointsWithin :: (Ix i, Coord i) => Int -> RT.RTree i a -> i -> [i]
pointsWithin n tree = \p -> filter ((<= n) . manhattan p)
                            . fmap fst
                            $ RT.query (RT.expandQuery n (p,p)) tree

numberOfConstellations :: [Point] -> Int
numberOfConstellations ps = let t = RT.index (zip ps (repeat ()))
                             in numberOfCliques $ searchGraph (pointsWithin 3 t) ps

test = do
  describe "parsing" $ do
    it "can round-trip example 1" $ do
      parseOnly inputP (textify example_1) `shouldBe` (Right example_1)
    it "can round-trip example 2" $ do
      parseOnly inputP (textify example_2) `shouldBe` (Right example_2)
    it "can round-trip example 3" $ do
      parseOnly inputP (textify example_3) `shouldBe` (Right example_3)
    it "can round-trip example 4" $ do
      parseOnly inputP (textify example_4) `shouldBe` (Right example_4)

  describe "pointsWithin" $ do
    let getTree :: [Point] -> RTree Point ()
        getTree ps = RT.index (zip ps (repeat ()))
        naive :: [Point] -> Point -> [Point]
        naive ps p = filter ((<= 3) . manhattan p) ps
    it "is the same as a naive filter" $ property $ \(NonEmpty points) -> do
      let t = getTree points
          p = head points
      S.fromList (pointsWithin 3 t p) == S.fromList (naive points p)
    describe "quick-check counter-examples" $ do
      describe "eg. 1" $ do
        let ps = [(0,0,0,0),(0,4,0,0),(0,-4,0,0)]
        it "queries correctly for the head" $ do
          pointsWithin 3 (getTree ps) (head ps) `shouldBe` naive ps (head ps)

  describe "numberOfConstellations" $ do
    describe "example_1" $ do
      it "has 2 constellations" $ do
        numberOfConstellations example_1 `shouldBe` 2
      it "could have only one, if we add a connecting point" $ do
        numberOfConstellations ((6,0,0,0) : example_1) `shouldBe` 1
    describe "example_2" $ do
      it "has 4 constellations" $ do
        numberOfConstellations example_2 `shouldBe` 4
    describe "example_3" $ do
      it "has 3 constellations" $ do
        numberOfConstellations example_3 `shouldBe` 3
    describe "example_4" $ do
      it "has 8 constellations" $ do
        numberOfConstellations example_4 `shouldBe` 8

textify ps = Text.unlines (to_string <$> ps)
  where
    to_string p = Text.intercalate "," (Text.pack . show . (p ^.) . runLens <$> RT.dimensions)

example_1 :: [Point]
example_1 =
  [(0,0,0,0)
  ,(3,0,0,0)
  ,(0,3,0,0)
  ,(0,0,3,0)
  ,(0,0,0,3)
  ,(0,0,0,6)
  ,(9,0,0,0)
  ,(12,0,0,0)
  ]

example_2 :: [Point]
example_2 =
  [(-1,2,2,0)
  ,(0,0,2,-2)
  ,(0,0,0,-2)
  ,(-1,2,0,0)
  ,(-2,-2,-2,2)
  ,(3,0,2,-1)
  ,(-1,3,2,2)
  ,(-1,0,-1,0)
  ,(0,2,1,-2)
  ,(3,0,0,0)
  ]

example_3 :: [Point]
example_3 =
  [(1,-1,0,1)
  ,(2,0,-1,0)
  ,(3,2,-1,0)
  ,(0,0,3,1)
  ,(0,0,-1,-1)
  ,(2,3,-2,0)
  ,(-2,2,0,0)
  ,(2,-2,0,-1)
  ,(1,-1,0,-1)
  ,(3,2,0,2)
  ]

example_4 :: [Point]
example_4 =
  [(1,-1,-1,-2)
  ,(-2,-2,0,1)
  ,(0,2,1,3)
  ,(-2,3,-2,1)
  ,(0,2,3,-2)
  ,(-1,-1,1,-2)
  ,(0,-2,-1,0)
  ,(-2,2,3,-1)
  ,(1,2,2,0)
  ,(-1,-2,0,-2)
  ]

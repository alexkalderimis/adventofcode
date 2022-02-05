module Elves.CliqueSpec (spec) where

import qualified Data.Array as A
import qualified Data.Set as S

import Test.Hspec
import Test.QuickCheck

import Elves.Clique

islands :: A.Array (Int,Int) Char
islands = A.listArray ((0,0), ub) (concat grid)
  where 
    ub = (length grid - 1, length (head grid) - 1)
    grid = [ "....x...x...xxxx."
           , "x..xxx..xxxxxx..."
           , "....x....x..xx.x."
           , "x........xxxx..x."
           , "x..xxx.........x."
           , "..............xx."
           ]

neighbours :: (Int,Int) -> [(Int,Int)]
neighbours (x,y) = filter (is (islands A.! (x,y)))
                        [(x - 1, y)
                        ,(x + 1, y)
                        ,(x, y - 1)
                        ,(x, y + 1)
                        ]

isLand :: (Int,Int) -> Bool
isLand = is 'x'

is c ix = A.inRange (A.bounds islands) ix && islands A.! ix == c

spec :: Spec
spec = describe "Elves.Clique" $ do
  let sg = searchGraph neighbours
      allSquares = A.range (A.bounds islands)
      landFall = filter isLand allSquares
      seaFall = filter (is '.') allSquares

  describe "numberOfCliques" $ do
    it "finds fewer cliques than squares" $ do
      numberOfCliques (sg allSquares) `shouldSatisfy` (< length allSquares)
    it "knows the number of islands, provided we start on land" $ do
      numberOfCliques (sg landFall) `shouldBe` 6
    it "is the same as length cliques" $ do
      let g = sg landFall
      numberOfCliques g `shouldBe` length (cliques g)
    it "knows the number of bodies of water, provided we start on water" $ do
      numberOfCliques (sg seaFall) `shouldBe` 3
    it "knows that all all = land + sea" $ do
      numberOfCliques (sg allSquares) `shouldBe` (numberOfCliques (sg landFall) + numberOfCliques (sg seaFall))

  describe "cliques" $ do
    specify "unions cliques === allSquares" $ do
      let g = sg allSquares
      foldl1 (<>) (cliques g) `shouldBe` clique (head $ searchGraph (const allSquares) allSquares)
    it "knows that all = land <> sea" $ do
      cliques (sg allSquares) `shouldMatchList` (cliques (sg landFall) <> cliques (sg seaFall))
    specify "no square is in more than one clique" $
      forAll (elements allSquares) $ \e -> do
        let cs = cliques (sg allSquares)
        filter (`member` e) cs `shouldSatisfy` ((== 1) . length)

  describe "clique" $ do
    it "knows the size of the island beginning at (0,8)" $ do
      length (clique . head $ sg [(0,8)]) `shouldBe` 18
    it "knows the size of the island beginning at (0,4)" $ do
      length (clique . head $ sg [(0,4)]) `shouldBe` 5

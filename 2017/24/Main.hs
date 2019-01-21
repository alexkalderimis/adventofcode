{-# LANGUAGE OverloadedStrings #-}

import           Data.Heap               (Heap)
import qualified Data.Heap               as Heap
import qualified Data.List               as L
import           Data.Maybe
import           Data.Ord
import qualified Data.Text               as Text
import           System.Exit

import           Data.Attoparsec.Text    (char, decimal)
import           Text.Parser.Char        (newline)
import           Text.Parser.Combinators (choice, sepBy1)

import           Elves
import           Elves.Advent

type Port = Word
data Component = Component { portA :: Port, portB :: Port } deriving (Show, Eq)

instance Ord Component where
  compare a b = compare (strength a, portA a, portB a) (strength b, portA b, portB b)

main :: IO ()
main = day 24 parser pt1 pt2 test
  where
    pt1 cs = case bestChain chainStrength (chains 0 cs) of
               Nothing    -> die "No chain found"
               Just chain -> print (chainStrength $ chain)
    pt2 cs = case bestChain (length <#> chainStrength) (chains 0 cs) of
               Nothing    -> die "No chain found"
               Just chain -> print (chainStrength $ chain)

test = do
  let mcomps = parseOnly parser exampleComponents
  describe "example" $ do
    it "pt1" $ do
      fmap (bestChain chainStrength. chains 0) mcomps
           `shouldBe` Right (Just [Component 0 1
                                  ,Component 1 10
                                  ,Component 9 10
                                  ])
    it "pt2" $ do
      fmap (bestChain (length <#> chainStrength) . chains 0) mcomps
           `shouldBe` Right (Just [Component 0 2
                                  ,Component 2 2
                                  ,Component 2 3
                                  ,Component 3 5
                                  ])

  describe "parser" $ do
    it "parses the example correctly" $ do
      mcomps `shouldBe` Right [Component 0 2
                              ,Component 2 2
                              ,Component 2 3
                              ,Component 3 4
                              ,Component 3 5
                              ,Component 0 1
                              ,Component 1 10
                              ,Component 9 10
                              ]

bestChain :: Ord b => ([Component] -> b) -> [[Component]] -> Maybe [Component]
bestChain f = listToMaybe . L.sortBy (comparing (Down . f))

chainStrength :: [Component] -> Word
chainStrength = sum . fmap strength

-- generates all possible chains going forward from the current port.
chains :: Port -> [Component] -> [[Component]]
chains p comps = do
  let cs = zip comps [0 ..]
  (next,p') <- [(c, port) | c <- L.sortBy (comparing Down) cs
                          , Just port <- [connectsTo p (fst c)]
               ]
  fmap (fst next :) ([] : chains p' [c | (c,x) <- cs, x /= snd next])

strength :: Component -> Word
strength (Component a b) = a + b

connectsTo :: Port -> Component -> Maybe Port
connectsTo p c
  | portA c == p = Just (portB c)
  | portB c == p = Just (portA c)
  | otherwise    = Nothing

parser :: Parser [Component]
parser = component `sepBy1` newline
  where
    component = do
      a <- decimal
      char '/'
      b <- decimal
      let [x,y] = L.sort [a,b]
      return (Component x y)

exampleComponents = Text.unlines
  ["0/2"
  ,"2/2"
  ,"2/3"
  ,"3/4"
  ,"3/5"
  ,"0/1"
  ,"10/1"
  ,"9/10"
  ]

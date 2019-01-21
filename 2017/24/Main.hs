{-# LANGUAGE OverloadedStrings #-}

import           Data.Heap               (Heap)
import qualified Data.Heap               as Heap
import qualified Data.List               as L
import           Data.Maybe
import           Data.Ord
import qualified Data.Text               as Text
import           System.Exit
import           Data.Monoid
import           Data.Tree (Forest, Tree(..))
import qualified Data.Tree as Tree

import           Data.Attoparsec.Text    (char, decimal)
import           Text.Parser.Char        (newline)
import           Text.Parser.Combinators (choice, sepBy1)

import           Elves
import           Elves.Advent

type Port = Word
data Component = Component { portA :: Port, portB :: Port } deriving (Show, Eq)
type Chain = [Component]

instance Ord Component where
  compare a b = compare (strength a, portA a, portB a) (strength b, portA b, portB b)

main :: IO ()
main = day 24 parser pt1 pt2 test
  where
    pt1 = print . highestStrength  . bridges 0
    pt2 = print . strongestLongest . bridges 0

test = do
  let mcomps = parseOnly parser exampleComponents
  describe "example" $ do
    it "pt1-find chain" $ do
      fmap (best chainStrength. chains 0) mcomps
           `shouldBe` Right (Just [Component 0 1
                                  ,Component 1 10
                                  ,Component 9 10
                                  ])
    it "pt1-find strength" $ do
      fmap (fmap chainStrength . best chainStrength. chains 0) mcomps
           `shouldBe` Right (Just 31)
    it "pt1-find strength from bridges" $ do
      fmap (highestStrength . bridges 0) mcomps
           `shouldBe` Right 31
    it "pt2-find chain" $ do
      fmap (best (length <#> chainStrength) . chains 0) mcomps
           `shouldBe` Right (Just [Component 0 2
                                  ,Component 2 2
                                  ,Component 2 3
                                  ,Component 3 5
                                  ])
    it "pt2-find strength" $ do
      fmap (fmap chainStrength . best (length <#> chainStrength) . chains 0) mcomps
           `shouldBe` Right (Just 19)
    it "pt2-find strength from bridges" $ do
      fmap (strongestLongest . bridges 0) mcomps
           `shouldBe` Right 19

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

highestStrength :: Bridges -> Word
highestStrength = getSum . bestTree (Sum . strength)

strongestLongest :: Bridges -> Word
strongestLongest = getSum . snd . bestTree ((Sum . const 1) <#> (Sum . strength))

chainStrength :: [Component] -> Word
chainStrength = sum . fmap strength

-- generates all possible chains going forward from the current port.
chains :: Port -> [Component] -> [Chain]
chains p comps = fromBridges (bridges p comps)
  where
    fromBridges bs = bs >>= \t -> fmap (rootLabel t :) ([] : fromBridges (subForest t))

type Bridges = Forest Component

bridges :: Port -> [Component] -> Bridges
bridges p comps = do
  let cs = zip comps [0 ..]
  (next,p') <- [ (c, port) | c <- cs , Just port <- [connectsTo p (fst c)] ]
  return $ Node (fst next) (bridges p' [c | (c,x) <- cs, x /= snd next])

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

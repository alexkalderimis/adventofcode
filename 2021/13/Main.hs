{-# LANGUAGE OverloadedStrings #-}

import Data.Bool
import qualified Data.List.Extra as L
import qualified Data.Text as T
import           Data.Text (Text)
import qualified Data.Attoparsec.Text as A
import           Control.Applicative.Combinators
import           Data.Attoparsec.Text            (decimal)
import           Text.Parser.Char (newline, text)
import qualified Data.Ix as Ix
import qualified Data.Set as Set
import           Data.Set (Set)

import Elves
import Elves.Advent
import Elves.StrictGrid

data Paper = Paper { bounds :: (Coord, Coord), dots :: Set Coord }
newtype Instruction = Instruction (Paper -> Paper)

instance Show Paper where
  show (Paper (a, b) ds) = let w = abs $ getCol (col b) - getCol (col a) + 1
                           in L.intercalate "\n"
                              . L.chunksOf w
                              . fmap (bool '.' '#' . flip Set.member ds)
                              $ Ix.range (a, b)

visibleDots :: Paper -> Set Coord
visibleDots (Paper bs dots) = Set.filter (Ix.inRange bs) dots

foldUp :: Row -> Paper -> Paper
foldUp   = foldPaper row getRow (\f (Row n) -> Row (f n)) (\c a -> c { row = a })

foldLeft :: Col -> Paper -> Paper
foldLeft = foldPaper col getCol (\f (Col n) -> Col (f n)) (\c a -> c { col = a })

foldPaper :: Enum a
          => (Coord -> a)
          -> (a -> Int)
          -> ((Int -> Int) -> (a -> a))
          -> (Coord -> a -> Coord)
          -> a -> Paper -> Paper
foldPaper dim int mapDim setDim a p
  = let (lb, ub) = bounds p
        bs  = (lb, setDim ub (pred a))
        bs' = (setDim lb (succ a), ub)
        reflection = Set.map (\c -> setDim c $ reflect (dim ub) (dim c))
                     . Set.filter (Ix.inRange bs')
                     $ dots p
                      
     in p { bounds = bs, dots = dots p <> reflection }
    where
      reflect maxa = mapDim (abs . subtract (int maxa))

runFolds :: Paper -> [Instruction] -> Paper
runFolds = L.foldl' (\p (Instruction i) -> i p)

main :: IO ()
main = day 13 parser pt1 pt2 test
  where
    pt1 (p, is) = print . Set.size . visibleDots . runFolds p $ take 1 is
    pt2 (p, is) = putStrLn . fmap (\c -> if c == '.' then ' ' else c) $ show (runFolds p is)

test = do
  let exampleInput = ["6,10"
                     ,"0,14"
                     ,"9,10"
                     ,"0,3"
                     ,"10,4"
                     ,"4,11"
                     ,"6,0"
                     ,"6,12"
                     ,"4,1"
                     ,"0,13"
                     ,"10,12"
                     ,"3,4"
                     ,"3,0"
                     ,"8,4"
                     ,"1,10"
                     ,"2,14"
                     ,"8,10"
                     ,"9,0"
                     ,""
                     ,"fold along y=7"
                     ,"fold along x=5"
                     ]
      Right (p, is) = parseOnly parser (T.intercalate "\n" exampleInput)
  it "can run instructions" $ do
    show (runFolds p is) `shouldBe` L.intercalate "\n"
                                                  [ "#####"
                                                  , "#...#"
                                                  , "#...#"
                                                  , "#...#"
                                                  , "#####"
                                                  , "....."
                                                  , "....."
                                                  ]
  describe "foldUp" $ do
    it "produces the correct result" $ do
      show (foldUp (Row 7) p) `shouldBe` L.intercalate "\n"
                                                       [ "#.##..#..#."
                                                       , "#...#......"
                                                       , "......#...#"
                                                       , "#...#......"
                                                       , ".#.#..#.###"
                                                       , "..........."
                                                       , "..........."
                                                       ]
  describe "foldLeft" $ do
    it "produces the correct result" $ do
      let p' = foldUp (Row 7) p
      show (foldLeft (Col 5) p') `shouldBe` L.intercalate "\n"
                                                       [ "#####"
                                                       , "#...#"
                                                       , "#...#"
                                                       , "#...#"
                                                       , "#####"
                                                       , "....."
                                                       , "....."
                                                       ]

letters :: Paper -> [String]
letters = fmap (L.intercalate "\n" . fmap (take 4) . L.transpose)
        . L.chunksOf 5
        . L.transpose
        . lines
        . show

parser :: Parser (Paper, [Instruction])
parser = do
  keys <- keyP `sepBy1` newline
  let dots = Set.fromList keys
      r = maximum (row <$> keys)
      c = maximum (col <$> keys)
  newline >> newline
  is <- instrP `sepBy1` newline
  pure (Paper { bounds = (origin, Coord r c), dots = dots} , is)
  where
    instrP = Instruction <$> do
      text "fold along "
      A.choice [foldUp . Row <$> (text "y=" >> decimal)
               ,foldLeft . Col <$> (text "x=" >> decimal)
               ]
    keyP = flip Coord <$> fmap Col (decimal <* text ",")
                      <*> fmap Row decimal

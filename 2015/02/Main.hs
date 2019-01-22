{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Text as Text
import Text.Parser.Char (newline)
import Text.Parser.Combinators (sepBy1)
import Data.Attoparsec.Text (decimal)

import Elves
import Elves.Advent

data Present = Present { w,h,l :: Word } deriving (Show, Eq)

main :: IO ()
main = day 2 parser pt1 pt2 test
  where
    pt1 = print . requiredArea
    pt2 = print . requiredRibbon

parser :: Parser [Present]
parser = present `sepBy1` newline
  where present = Present <$> decimal <*> ("x" *> decimal) <*> ("x" *> decimal)

requiredArea :: [Present] -> Word
requiredArea = sum . fmap (liftA2 (+) area smallestSide)

requiredRibbon :: [Present] -> Word
requiredRibbon = sum . fmap (liftA2 (+) bow smallestPerimeter)

area = sum . fmap (2 *) . sides

smallestSide = minimum . sides

smallestPerimeter = minimum . perimeters

bow Present{..} = w * h * l

sides Present{..} = [ x * y | (x,y) <- pairs [w,h,l] ]

perimeters Present{..} = [2 * x + 2 * y | (x,y) <- pairs [w,h,l]]

test = do
  let summed egs = (("total", Text.unlines (fst <$> egs), sum (snd <$> egs)) : namedExamples egs)
  describe "required-ribbon" $ do
    let examples = [("2x3x4", 34)
                   ,("1x1x10", 14)
                   ]
    testing "calculates the correct length for" (requiredRibbon <$> parser) (summed examples)

  describe "required-area" $ do
    let examples = [("2x3x4", 58)
                   ,("1x1x10", 43)
                   ]
    testing "calculates the correct sq-footage for" (requiredArea <$> parser) (summed examples)


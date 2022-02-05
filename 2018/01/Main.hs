import Data.Monoid
import Data.List (intercalate)
import qualified Data.IntSet as Set
import           Text.Parser.Char (newline)
import qualified Data.Attoparsec.Text as A
import           Control.Applicative.Combinators

import Elves
import Elves.Advent

main :: IO ()
main = day 1 parser pt1 pt2 (pure ())
  where
    pt1 = print . sum
    pt2 = print . firstRepeated . cycle
    parser = A.signed A.decimal `sepBy1` newline 

firstRepeated :: [Int] -> Maybe Int
firstRepeated = go (Set.singleton 0, 0)
  where
    go _ [] = Nothing
    go (seen, curr) (x:xs) =
         let next = curr + x
          in if Set.member next seen
                then Just next
                else go (Set.insert next seen, next) xs

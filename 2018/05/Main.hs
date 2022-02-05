import Data.Char

import qualified Data.Attoparsec.Text as A
import qualified Data.Text as T

import Elves.Advent

main :: IO ()
main = day 5 (T.unpack <$> A.takeText) pt1 pt2 spec
  where
    pt1 = print . rl
    pt2 s = do
      let optimised = do
           c <- ['a' .. 'z']
           let candidate = filter (`notElem` [c, toUpper c]) s
           return (c, rl candidate)
      putStrLn . ("Minimum: " ++) . show . minimum . fmap snd $ optimised

    spec = pure ()
    rl = length . react

-- react the string by collapsing sequences of 'reactive' characters (defined
-- as characters of same letter by opposite case). We traverse the string
-- maintaining a stack of past characters to enable back-tracking once we
-- delete a pair, as deletions can provoke further deletions.
react :: String -> String
react = go []
  where go past []       = reverse past
        go past (x:y:cs) | reactive x y = case past of []    -> go [] cs
                                                       (c:p) -> go p  (c:cs)
        go past (c:cs)   = go (c:past) cs

reactive :: Char -> Char -> Bool
reactive a b = a /= b && toUpper a == toUpper b

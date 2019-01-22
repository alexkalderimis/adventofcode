import qualified Data.Set as S
import Data.Attoparsec.Text (letter, digit)
import Text.Parser.Combinators (sepBy1)
import Text.Parser.Char (newline)

import Elves
import Elves.Advent

type Rule = String -> Bool
type Rules = [Rule]

main :: IO ()
main = day 5 (some (letter <|> digit) `sepBy1` newline) pt1 pt2 test
  where
    pt1 = print . length . filter (isNice pt1Rules)
    pt2 = print . length . filter (isNice pt2Rules)

test = do
  let itShouldBeNice    rs it = which "is nice" (it `shouldSatisfy` isNice rs)
  let itShouldBeNaughty rs it = which "is naughty" (it `shouldNotSatisfy` isNice rs)
  describe "pt1" $ do
    let rs = pt1Rules
    consider "ugknbfddgicrmopn" (itShouldBeNice rs)
    consider "aaa"              (itShouldBeNice rs)
    consider "jchzalrnumimnmhp" (itShouldBeNaughty rs)
    consider "haegwjzuvuyypxyu" (itShouldBeNaughty rs)
    consider "haegwjzuvuyypxu"  (itShouldBeNice rs)
    consider "dvszwmarrgswjxmb" (itShouldBeNaughty rs)
  describe "pt2" $ do
    let rs = pt2Rules
    consider "qjhvhtzxzqqjkmpb" (itShouldBeNice rs)
    consider "xxyxx"            (itShouldBeNice rs)
    consider "uurcxstgmygtbstg" (itShouldBeNaughty rs)
    consider "ieodomkazucvgmuy" (itShouldBeNaughty rs)

isNice :: Rules -> String -> Bool
isNice rs s = all ($ s) rs

pt2Rules :: Rules
pt2Rules = [rule_a, rule_b]
  where
    rule_a (a:b:cs) = occursIn 2 [a,b] cs || rule_a (b:cs)
    rule_a _        = False

    rule_b s@(a:_:b:_) = a == b || rule_b (tail s)
    rule_b _           = False

    occursIn n a str = let b = take n str
                        in if length b < n then False
                                           else a == b || occursIn n a (tail str)


pt1Rules :: Rules
pt1Rules = [rule_a, rule_b, rule_c]
  where
    rule_a = (> 2) . length . take 3 . filter (flip S.member vowels)

    vowels = S.fromList "aeiou"

    rule_b (a:b:xs) = if a == b then True else rule_b (b:xs)
    rule_b _        = False

    rule_c (a:b:xs) = if S.member [a,b] forbidden then False
                                                  else rule_c (b:xs)
    rule_c _ = True

    forbidden = S.fromList ["ab","cd","pq","xy"]

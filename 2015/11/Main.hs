import qualified Data.Attoparsec.Text as A

import Elves
import Elves.Advent

main :: IO ()
main = day 11 parser pt1 pt2 test
  where
    parser = (some A.letter)
    pt1 = print . nextPassword
    pt2 = print . nextPassword . nextPassword

test = do
  describe "nextPassword" $ do
    specify "The next password after abcdefgh is abcdffaa" $ do
      nextPassword "abcdefgh" `shouldBe` "abcdffaa"
    specify "The next password after ghijklmn is ghjaabcc" $ do
      nextPassword "ghijklmn" `shouldBe` "ghjaabcc"

nextPassword :: String -> String
nextPassword = head -- we only want the first one
             . dropWhile (not . legal) -- ignore anything that doesn't match the rules
             . drop 1 -- ignore the input itself
             . iterate (reverse . incr . reverse) -- get all the bumped strings
             . unforbid -- remove forbidden characters, following the endian rules

legal s = hasStraight s && hasTwoRuns s

unforbid [] = []
unforbid (x:xs) = let x' = bumpForbidden x
                   in if x == x' then x : unforbid xs
                                 else x' : zipWith pure (repeat 'a') xs

hasStraight s = case s of
  (a:b:c:xs) | b == succ a && c == succ b -> True
  (_:xs) -> hasStraight xs
  [] -> False

hasTwoRuns s = maybe False (pure True) (splitAfterRun (Nothing, s) >>= splitAfterRun)
  where
    splitAfterRun (_, []) = Nothing
    splitAfterRun (mc, a:b:xs) | a == b && maybe True (/= a) mc = Just (Just a, xs)
    splitAfterRun (mc, _:xs) = splitAfterRun (mc,xs)

incr [] = ['a']
incr (x:xs) = let x' = succ' x
               in if x' < x then x' : incr xs
                            else (x' : xs)

succ' c = let c' = succ c in if c' > 'z' then 'a' else bumpForbidden c'

bumpForbidden c = if c `elem` ['i', 'o', 'l'] then succ' c else c

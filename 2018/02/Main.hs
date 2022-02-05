import Data.List.Extra
import Data.Attoparsec.Text (takeText)
import qualified Data.Text as T

import Elves
import Elves.Advent

main :: IO ()
main = day 2 parser pt1 pt2 spec
  where
    pt1 lines = let (twos,threes) = unzip . fmap twosAndThrees $ lines
                    checksum = getSum (mconcat twos) * getSum (mconcat threes)
                 in putStrLn $ "Checksum: " <> show checksum

    pt2 = mapM_ putStrLn . commonElemsOfboxIdsDifferingBy 1

    parser = fmap T.unpack . T.lines <$> takeText

commonElemsOfboxIdsDifferingBy :: Int -> [String] -> [String]
commonElemsOfboxIdsDifferingBy n lines
  = nubOrd $ sort [ common boxA boxB | (boxA,boxB) <- pairs lines
                                     , diffsAtPosition boxA boxB == n
                                     ]

twosAndThrees :: String -> (Sum Int, Sum Int)
twosAndThrees input = (findSum 2 groupCounts, findSum 3 groupCounts)
  where
    groupCounts = filter (\x -> x == 2 || x == 3) . fmap length . group $ sort input
    findSum n = foldMap (pure (Sum 1)) . find (== n)

diffsAtPosition as bs = getSum . mconcat $ zipWith f as bs
  where
    f a b | a == b = Sum 0
    f _ _          = Sum 1

common :: Eq a => [a] -> [a] -> [a]
common as bs = zip as bs >>= \(a,b) -> [a | a == b]

spec = do
  describe "twosAndThrees" $ do
    specify "abcdef contains no letters that appear exactly two or three times." $ do
      twosAndThrees "abcdef" `shouldBe` (mempty, mempty)
    specify "bababc contains two a and three b, so it counts for both." $ do
      twosAndThrees "bababc" `shouldBe` (Sum 1, Sum 1)
    specify "abbcde contains two b, but no letter appears exactly three times." $ do
      twosAndThrees "abbcde" `shouldBe` (Sum 1, mempty)
    specify "abcccd contains three c, but no letter appears exactly two times." $ do
      twosAndThrees "abcccd" `shouldBe` (mempty, Sum 1)
    specify "aabcdd contains two a and two d, but it only counts once." $ do
      twosAndThrees "aabcdd" `shouldBe` (Sum 1, mempty)
    specify "abcdee contains two e." $ do
      twosAndThrees "abcdee" `shouldBe` (Sum 1, mempty)
    specify "ababab contains three a and three b, but it only counts once." $ do
      twosAndThrees "ababab" `shouldBe` (mempty, Sum 1)
  describe "diffsAtPosition" $ do
    it "knows fghij and fguij differ by one" $ do
      diffsAtPosition "fguij" "fghij" `shouldBe` 1
    it "knows abcde and axcye differ by two" $ do
      diffsAtPosition "abcde" "axcye" `shouldBe` 2
  describe "commonElemsOfboxIdsDifferingBy" $ do
    let lines = [ "abcde"
                , "fghij"
                , "klmno"
                , "pqrst"
                , "fguij"
                , "axcye"
                , "wvxyz"
                ]
    it "can be used to find the correct answer" $ do
      commonElemsOfboxIdsDifferingBy 1 lines `shouldBe` ["fgij"]

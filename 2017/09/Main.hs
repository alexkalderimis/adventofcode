{-# LANGUAGE OverloadedStrings #-}

import           Control.Monad
import           Data.Attoparsec.Text    (Parser, parseOnly)
import qualified Data.Attoparsec.Text    as Atto
import           Data.Either             (partitionEithers)
import           Data.Maybe
import           Data.Semigroup
import           Data.Text               (Text)
import qualified Data.Text               as Text
import qualified Data.Text.IO            as Text
import           System.Exit
import           Test.Hspec
import           Test.QuickCheck         (Arbitrary, arbitrary, property)
import           Text.Parser.Char
import           Text.Parser.Combinators (between, choice, sepBy)
import           Text.Printf

import           Data.Tree

type Group = Tree [Int]

main :: IO ()
main = do
  t <- Text.getContents
  case parseOnly groupP t of
    Left err -> die err
    Right g -> do printf "SCORE: %d\n" (score 1 g)
                  printf "GARBAGE: %d\n" (countGarbage g)


countGarbage :: Group -> Int
countGarbage = getSum . foldMap (Sum . sum)

countGroups :: Group -> Int
countGroups g = 1 + sum (fmap countGroups $ subForest g)

score :: Int -> Group -> Int
score outer g = case subForest g of
  ns -> outer + sum (fmap (score (outer + 1)) ns)

groupP :: Parser Group
groupP = between (char '{') (char '}') $ do
  contents <- groupOrGarbage `sepBy` char ','
  let (garbage, groups) = partitionEithers contents
  return (Node garbage groups)

groupOrGarbage :: Parser (Either Int Group)
groupOrGarbage = choice [Right <$> groupP
                        ,Left  <$> garbageP
                        ]

garbageP :: Parser Int
garbageP = char '<' >> go 0
  where
    go n = do
      c <- anyChar
      case c of
        '>' -> return n
        '!' -> anyChar >> go n
        _   -> go (n + 1)

spec :: Spec
spec = do
  describe "score" $ do
    let table = [("{}", 1)
                ,("{{{}}}", 6)
                ,("{{},{}}", 5)
                ,("{{{},{},{{}}}}", 16)
                ,("{<a>,<a>,<a>,<a>}", 1)
                ,("{{<ab>},{<ab>},{<ab>},{<ab>}}", 9)
                ,("{{<!!>},{<!!>},{<!!>},{<!!>}}", 9)
                ,("{{<a!>},{<a!>},{<a!>},{<ab>}}", 3)
                ]
    forM_ table $ \(txt, expected) -> do
       it ("should score " ++ Text.unpack txt ++ " correctly") $ do
         let r = parseOnly groupP txt
         fmap (score 1) r `shouldBe` Right expected
  describe "groupP" $ do
    it "can parse the empty group" $ do
      let r = parseOnly groupP "{}"
      r `shouldBe` Right (Node [] [])
    it "can parse nested groups" $ do
      let r = parseOnly groupP "{{{}}}"
      r `shouldBe` Right (Node []  [Node [] [Node [] []]])
    it "can parse comma separated nested groups" $ do
      let r = parseOnly groupP "{{},{}}"
      r `shouldBe` Right (Node [] [Node [] []
                                  ,Node [] []
                                  ])
    it "can parse more complex nestings" $ do
      let r = parseOnly groupP "{{{},{},{{}}}}"
      fmap countGroups r `shouldBe` Right 6
    it "can parse groups with garbage" $ do
      let r = parseOnly groupP "{<{},{},{{}}>}"
      r `shouldBe` Right (Node [10] [])
    it "can parse lots of different garbages" $ do
      let r = parseOnly groupP "{<a>,<a>,<a>,<a>}"
      r `shouldBe` Right (Node [1,1,1,1] [])
    it "can parse lots of different garbages, in sub-groups" $ do
      let r = parseOnly groupP "{{<a>},{<a>},{<a>},{<a>}}"
      fmap countGroups r `shouldBe` Right 5
    it "can handle cancelled characters" $ do
      let r = parseOnly groupP "{{<!>},{<!>},{<!>},{<a>}}"
      fmap countGroups r `shouldBe` Right 2

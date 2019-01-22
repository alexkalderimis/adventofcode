{-# LANGUAGE OverloadedStrings #-}

import           Crypto.Hash
import           Data.Attoparsec.Text (anyChar, manyTill)
import           Data.ByteString      (ByteString)
import qualified Data.ByteString      as B
import           Data.String
import           Text.Parser.Char     (newline)

import           Elves
import           Elves.Advent

main :: IO ()
main = day 4 (manyTill anyChar newline) pt1 pt2 test
  where
    pt1 = print . nextCoin 5
    pt2 = print . nextCoin 6

test = do
  describe "nextCoin" $ do
    consider "abcdef" $ \it ->
      which "has the next coin 609043" $ do
        nextCoin 5 it `shouldBe` 609043
  describe "md5" $ do
   consider "abcdef609043" $ \it -> do
     which "is a coin 5" $ do
       it `shouldSatisfy` isCoin 5
     which "is a coin 4" $ do
       it `shouldSatisfy` isCoin 4
     which "is not a coin 6" $ do
       it `shouldNotSatisfy` isCoin 6
   consider "abcdef60904" $ \it -> do
     which "is not a coin" $ do
       it `shouldNotSatisfy` isCoin 5

nextCoin :: Int -> String -> Int
nextCoin n pref = head . filter (makesCoin n) $ [1 .. ]
  where
    makesCoin n = isCoin n . fromString . (pref ++) . show

isCoin :: Int -> ByteString -> Bool
isCoin n bs = all (== '0') $ take n (hex $ md5 bs)

md5 :: ByteString -> Digest MD5
md5 = hash

hex :: Digest MD5 -> String
hex = show

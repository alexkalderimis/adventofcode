{-# LANGUAGE OverloadedStrings #-}

import           Control.Applicative.Combinators
import           Data.Attoparsec.Text            (decimal)
import qualified Data.Attoparsec.Text            as A
import           Data.Char
import           Data.Monoid
import           Data.Text                       (Text)
import qualified Data.Text                       as T
import           System.Exit

import           Elves
import           Elves.Advent

data Token = Simple Char
           | BackSlash
           | DblQuote
           | HexChar Char Char
           deriving (Show, Eq)

main :: IO ()
main = day 8 parser pt1 pt2 test
  where
    parser = T.lines <$> A.takeText
    len = T.length
    pt1 ts = case traverse (parseOnly decode) ts of
               Left err -> die err
               Right decoded ->
                 print $ sum (len <$> ts) - length (concat decoded)

    pt2 = print . getSum . foldMap (Sum . liftA2 subtract len (len . encode))

test = do
  let examples = [("\"\"", 2, 0, 6)
                 ,("\"abc\"", 5, 3, 9)
                 ,("\"aaa\\\"aaa\"", 10, 7, 16)
                 ,("\"\\x27\"", 6, 1, 11)
                 ]
  describe "decode" $ do
    forM_ examples $ \(str, rep_len, data_len, _) -> do
      describe (T.unpack str) $ do
        it (unwords ["has", show rep_len, "characters as text"]) $ do
          T.length str `shouldBe` rep_len
        it (unwords ["has", show data_len, "characters as data"]) $ do
          fmap length (parseOnly decode str) `shouldBe` Right data_len
  describe "encode" $ do
    forM_ examples $ \(str, _, _, dbl_encoded_len) -> do
      describe (T.unpack str) $ do
        it (unwords ["has", show dbl_encoded_len, "when encoded"]) $ do
          T.length (encode str) `shouldBe` dbl_encoded_len

encode :: Text -> Text
encode t = mconcat (dblQuote : fmap encodeToken (T.unpack t) ++ [dblQuote])
  where
    dblQuote = "\""
    bkSlash = "\\"
    encodeToken c = case c of
      '\\' -> T.replicate 2 bkSlash
      '\"' -> bkSlash <> dblQuote
      c    -> T.singleton c

decode :: Parser [Token]
decode = between (A.string dblQuote) (A.string dblQuote) (many chr)
  where
    bkSlash = "\\"
    dblQuote = "\""
    hex = A.satisfy isHexDigit
    chr = choice [BackSlash <$ A.string (bkSlash <> bkSlash)
                 ,DblQuote  <$ A.string (bkSlash <> dblQuote)
                 ,A.string (bkSlash <> "x") *> (HexChar <$> hex <*> hex)
                 ,A.string bkSlash *> fail "unexpected escape"
                 ,Simple <$> A.notChar '\"'
                 ]


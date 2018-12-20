{-# LANGUAGE OverloadedStrings #-}

import           Control.Applicative
import           Data.Attoparsec.Text    (Parser, parseOnly)
import qualified Data.Map.Strict         as M
import           Data.Maybe
import           Data.Text               (Text)
import qualified Data.Text               as Text
import qualified Data.Text.IO            as Text
import qualified Data.Vector             as V
import           System.Exit
import           Text.Parser.Char
import           Text.Parser.Combinators
import           Text.Parser.Token
import           Text.Printf

type Range = Int
type Depth = Int
type Picos = Int
type Scanners = M.Map Depth (Range, V.Vector Int)

main :: IO ()
main = do
  inp <- parseOnly inputP <$> Text.getContents
  case inp of
    Left err -> die err
    Right scs -> do printf "Severity without delay: %d\n" (severity scs 0)
                    printf "Minimum safe delay: %d picos\n" (minDelay scs)

inputP :: Parser Scanners
inputP = fmap (fmap withCycle . M.fromList)
       $ ((,) <$> int <*> (string ": " *> int))
          `sepBy1` newline
  where int = fromInteger <$> decimal
        withCycle r = (r, V.fromList (positions r))

posAtTime :: Picos -> V.Vector Int -> Int
posAtTime t cycle = cycle V.! (t `mod` V.length cycle)

positions :: Range -> [Int]
positions r = let fwd = take r [0 .. ]
                  bwd = reverse $ take (r - 2) [1 ..]
               in fwd ++ bwd

severity :: Scanners -> Picos -> Int
severity scs t0 = sum (catches scs t0)

catches :: Scanners -> Picos -> [Int]
catches scs t0 = [d * r | (d,(r,ps)) <- M.toList scs
                        , posAtTime (t0 + d) ps == 0
                        ]

minDelay :: Scanners -> Picos
minDelay scs = head $ dropWhile (not . null . catches scs) [0 ..]

exampleInput = Text.unlines
  ["0: 3"
  ,"1: 2"
  ,"4: 4"
  ,"6: 4"
  ]

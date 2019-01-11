{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Array         as A
import           Data.Bool
import qualified Data.ByteString    as B
import           Data.Foldable      (foldMap)
import qualified Data.Map.Strict    as M
import           Data.Monoid
import           Data.Word
import           System.Environment
import           System.Exit

import           Elves.Cartesian
import           Elves.Clique
import qualified Elves.Knot         as Knot

type FragLine = [Bool]

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["pt1"] -> pt1
    ["pt2"] -> pt2
    _       -> die "bad arguments, expected pt1, or pt2"

pt1 = B.getContents >>= print . usedSquares

pt2 = do
  inpt <- B.getContents
  let bs = ((0,0), (127,127))
      used = A.listArray bs (mconcat $ frag inpt)
      neighbours pos = [ p | p <- ($ pos) <$> [up,down,left,right]
                       , A.inRange bs p && used A.! p
                       ]
  print . numberOfCliques
        . searchGraph neighbours
        . filter (used A.!)
        $ A.range bs

usedSquares :: B.ByteString -> Int
usedSquares = sum . fmap (bool 0 1) . mconcat . frag

fragSection :: A.Array Word8 FragLine
fragSection = A.listArray (minBound,maxBound)
  [[a,b,c,d,e,f,g,h] | a <- ds, b <- ds, c <- ds, d <- ds
                     , e <- ds, f <- ds, g <- ds, h <- ds
  ]
  where ds = [False, True] -- ".#" -- "01"

fragLine :: B.ByteString -> FragLine
fragLine = foldMap (fragSection A.!) . Knot.hashl

frag :: B.ByteString -> [FragLine]
frag input = fmap fragLine . fmap (input <>)  $ suffixes
  where
    suffix n = "-" <> B.pack (toEnum . fromEnum <$> show n)
    suffixes = [suffix n | n <- [0 .. 127]]

showFragLine :: FragLine -> String
showFragLine = fmap (bool '.' '#')

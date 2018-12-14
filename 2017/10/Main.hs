{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Array         as A
import           Data.Bits          (Bits, shiftR, xor)
import qualified Data.ByteString    as B
import           Data.Foldable      hiding (toList)
import           Data.Word
import           System.Environment
import           System.Exit
import           Test.Hspec

data Zipper a = Zipper
  { idx    :: Int
  , lefts  :: [a]
  , focus  :: a
  , rights :: [a]
  } deriving (Show, Eq, Functor)

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["pt1"]  -> pt1
    ["pt2"]  -> pt2
    ["test"] -> withArgs [] (hspec spec)
    _        -> die "bad arguments, expected pt1, or pt2"

pt1 :: IO ()
pt1 = do
  -- this is possibly the dumbest possible way to parse input
  -- only acceptable because we know exactly what the input is.
  lens <- read . ('[':) . (++ "]") <$> getContents
  print (pinchHash 256 lens)

pt2 :: IO ()
pt2 = B.interact hash >> putStrLn ""

hash :: B.ByteString -> B.ByteString
hash input =
  let salt  = [17, 31, 73, 47, 23]
      bytes = B.unpack input ++ salt
      n     = B.length input + 5
      p z (skip, len) = pinch 256 len skip z
   in B.pack . hexadecimal . dense
             . toList . rewind
             . foldl' p (fromList [0 .. 255])
             . zip [0 ..]
             . take (n * 64)
             . cycle
             $ fmap fromIntegral bytes

pinchHash :: Int -> [Int] -> Int
pinchHash n
  = product . take 2 . toList . rewind . foldl' pinch' z0 . zip [0 ..]
  where
    z0 = fromList (take n [0 ..])
    pinch' z (skip, len) = pinch n len skip z

dense :: Bits byte => [byte] -> [byte]
dense [] = []
dense xs = let (as,bs) = splitAt 16 xs
                in foldl1 xor as : dense bs

hexChars :: A.Array Word8 Word8
hexChars = A.listArray (0,15)
  $ fmap (toEnum . fromEnum) (['0' .. '9'] ++ ['a' .. 'f'])

hexadecimal :: [Word8] -> [Word8]
hexadecimal = (>>= byteToHex)
  where byteToHex b = let h = b `shiftR` 4
                          l = b `mod` 16
                       in [hexChars A.! h, hexChars A.! l]

fromList :: [a] -> Zipper a
fromList []     = error "cannot build zipper for empty list"
fromList (a:as) = Zipper 0 [] a as

left :: Zipper a -> Zipper a
left (Zipper i [] a [])         = Zipper i [] a []
left (Zipper i [] a rs)         = left (Zipper i (reverse rs) a [])
left (Zipper i (new:ls) old rs) = Zipper (pred i) ls new (old:rs)

right :: Zipper a -> Zipper a
right (Zipper i [] a [])         = Zipper i [] a []
right (Zipper i ls a [])         = right (Zipper i [] a (reverse ls))
right (Zipper i ls old (new:rs)) = Zipper (succ i) (old:ls) new rs

shift :: Int -> Zipper a -> Zipper a
shift 0 z = z
shift n z
  | n < 0     = shift (succ n) (left z)
  | otherwise = shift (pred n) (right z)

shiftTo :: Int -> Zipper a -> Zipper a
shiftTo i z = shift (i - idx z) z

toList :: Zipper a -> [a]
toList z = focus z : rights z ++ reverse (lefts z)

-- perform a round of the pinch hash.
-- This takes the length of the zipper to avoid having to recalculate it
-- each round.
pinch :: Int -> Int -> Int -> Zipper a -> Zipper a
pinch zlen len skip z
  = let (pinched, rst) = splitAt len (toList z)
        z'             = (fromList (reverse pinched ++ rst)) { idx = idx z }
        idx'           = (idx z + len + skip) `mod` zlen
    in shiftTo idx' z'

rewind :: Zipper a -> Zipper a
rewind = shiftTo 0

spec :: Spec
spec = do
  describe "asciiHash" $ do
    it "can hash the empty string" $ do
      hash "" `shouldBe` "a2582a3a0e66e6e86e3812dcb672a272"
    it "can hash AoC 2017" $ do
      hash "AoC 2017" `shouldBe` "33efeb34ea91902bb2f59c9920caa6cd"
    it "can hash 1,2,3" $ do
      hash "1,2,3" `shouldBe` "3efbe78a8d82f29979031a4aa0b16a9d"
    it "can hash 1,2,4" $ do
      hash "1,2,4" `shouldBe` "63960835bcdc130f0b66d7ff4f6a5a8e"


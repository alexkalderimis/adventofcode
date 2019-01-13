module Elves.Knot where

import qualified Data.Array         as A
import           Data.Foldable      hiding (toList)
import           Data.Bits          (Bits, shiftR, xor)
import qualified Data.ByteString    as B
import           Data.Word

import qualified Elves.CircularBuffer as CB
import           Elves.CircularBuffer (Buffer(..))
import qualified Data.Sequence as Seq

byteZipper :: Buffer Word8
byteZipper = CB.fromList [0 .. 255]

hash :: B.ByteString -> B.ByteString
hash = B.pack . hexadecimal . hashl

hashl :: B.ByteString -> [Word8]
hashl input =
  let salt  = [17, 31, 73, 47, 23]
      bytes = B.unpack input ++ salt
      n     = B.length input + 5
      p z (skip, len) = pinch len skip z
   in dense . CB.toList
            . foldl' p byteZipper
            . zip [0 ..]
            . take (n * 64)
            . cycle
            $ fmap fromIntegral bytes

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

-- perform a round of the pinch hash.
pinch :: Int -> Int -> Buffer a -> Buffer a
pinch len skip b
  = let (pinched, rst) = CB.splitAt len b
        b'             = Seq.reverse (CB.getBuffer pinched) <> (CB.getBuffer rst)
        (rs,ls)        = Seq.splitAt (Seq.length b' - CB.bufIdx b) b'
     in CB.shift (len + skip) b { CB.getBuffer = ls <> rs }


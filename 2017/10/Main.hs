{-# LANGUAGE DeriveFunctor #-}

import           Data.Bits          (shiftR, xor)
import           Data.Foldable      hiding (toList)
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
    ["pt1"] -> pt1
    ["pt2"] -> pt2
    _       -> die "bad arguments, expected pt1, or pt2"

pt1 :: IO ()
pt1 = do
  -- this is possibly the dumbest possible way to parse input
  lens <- read . ('[':) . (++ "]") <$> getContents
  print (pinchHash 256 lens)

pt2 :: IO ()
pt2 = getContents >>= putStrLn . asciiHash . concat . lines

asciiHash :: String -> String
asciiHash input =
  let salt = [17, 31, 73, 47, 23]
      bytes = fmap ((`mod` 256) . fromEnum) input ++ salt
      hash z (skip, len) = pinch len skip z
      n = length bytes
   in hexadecimal . dense
                 . toList . rewind
                 . foldl' hash (fromList [0 .. 255])
                 . zip [0 ..]
                 $ take (n * 64) (cycle bytes)

pinchHash :: Int -> [Int] -> Int
pinchHash n
  = product . take 2 . toList . rewind . foldl' pinch' z0 . zip [0 ..]
  where
    z0 = fromList (take n [0 ..])
    pinch' z (skip, len) = pinch len skip z

dense :: [Int] -> [Int]
dense [] = []
dense xs = let (as,bs) = splitAt 16 xs
                in foldl1 xor as : dense bs

hexadecimal :: [Int] -> String
hexadecimal = (>>= byteToHex)
  where byteToHex b = let h = b `shiftR` 4
                          l = b `mod` 16
                       in [chars !! h, chars !! l]
        chars = ['0' .. '9'] ++ ['a' .. 'f']

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

toList :: Zipper a -> [a]
toList z = focus z : rights z ++ reverse (lefts z)

pinch :: Int -> Int -> Zipper a -> Zipper a
pinch len skip (Zipper i ls a rs)
  = let xs = a : rs ++ reverse ls
        (pinched, rst) = splitAt len xs
        z = (fromList (reverse pinched ++ rst)) { idx = i }
    in shift ((len + skip) `mod` length xs) z

rewind :: Zipper a -> Zipper a
rewind z = shift (negate $ idx z) z

spec :: Spec
spec = do
  describe "asciiHash" $ do
    it "can hash the empty string" $ do
      asciiHash "" `shouldBe` "a2582a3a0e66e6e86e3812dcb672a272"
    it "can hash AoC 2017" $ do
      asciiHash "AoC 2017" `shouldBe` "33efeb34ea91902bb2f59c9920caa6cd"
    it "can hash 1,2,3" $ do
      asciiHash "1,2,3" `shouldBe` "3efbe78a8d82f29979031a4aa0b16a9d"
    it "can hash 1,2,4" $ do
      asciiHash "1,2,4" `shouldBe` "63960835bcdc130f0b66d7ff4f6a5a8e"


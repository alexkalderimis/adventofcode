{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

import           Control.Monad.ST
import           Data.Array              (Ix)
import           Data.Array.MArray       (readArray, writeArray)
import qualified Data.Array.MArray       as MA
import           Data.Array.ST           (STUArray, runSTUArray)
import           Data.Array.Unboxed      (UArray)
import qualified Data.Array.Unboxed      as A
import           Data.Attoparsec.Text    (letter)
import qualified Data.Map.Strict         as M
import           Data.Maybe
import           Data.Word
import           Text.Parser.Char        (char, digit, newline)
import           Text.Parser.Combinators (sepBy1)
import           Text.Parser.Token       (comma)

import           Elves
import           Elves.Advent
import           Test.QuickCheck

type Position = Word8
type Dancer = Char
type Dancefloor = UArray Position Dancer

data DanceMove
  = Spin Position
  | Exchange Position Position
  | Partner Dancer Dancer
  deriving (Show,Eq,Ord)

type Dance = [DanceMove]

main :: IO ()
main = day 16 inputP pt1 pt2 test
  where
    init = A.listArray (0,15) ['a' .. 'p']
    pt1 moves = print . A.elems $ dance moves init
    pt2 moves = let target = 10 ^ 9
                    go seen n dancers
                      | n > target  = error "logic error - too many steps!"
                      | n == target = dancers
                      | otherwise   = let dancers' = dance moves dancers
                                          completed = n + 1
                                       in case M.lookup dancers' seen of
                                           Just n' -> let step = completed - n'
                                                          remaining = target - completed
                                                          ffwd = completed + step * (remaining `quot` step)
                                                       in go seen ffwd dancers'
                                           Nothing -> go (M.insert dancers' completed seen) completed dancers'
                in print . A.elems $ go (M.singleton init 0) 0 init

inputP = (danceMoveP `sepBy1` comma) <* optional newline
  where
    danceMoveP =    ("s" *> (Spin     <$> pos))
                <|> ("x" *> (Exchange <$> pos    <*> ("/" *> pos)))
                <|> ("p" *> (Partner  <$> dancer <*> ("/" *> dancer)))
    pos = read <$> some digit
    dancer = letter

test = do
  let dancers = A.listArray (0,4)
  describe "parsing" $ do
    it "parses the example moves" $ do
      parseOnly inputP "s1,x3/4,pe/b" `shouldBe` Right [Spin 1, Exchange 3 4, Partner 'e' 'b']
  describe "Spin" $ do
    it "never produces indices out of range" $ property $ \(NonNegative n, NonNegative i) ->
      spin (0,15) n (i `mod` 16 :: Position) `elem` [0 .. 15]
    it "produces the correct re-ordering" $ do
      (spin (0,4) 1 <$> [0 .. 4]) `shouldBe` [1,2,3,4,0]
    it "rotates the dancefloor" $ do
      dance [Spin 1] (dancers "abcde") `shouldBe` dancers "eabcd"
    it "is idempotent to spin N times" $ property $ \(Positive n) ->
      let init = A.listArray (0, fromIntegral (n - 1)) (take n ['a' ..])
       in dance (take n (repeat (Spin 1))) init == init
  describe "Exchange" $ do
    it "swaps the position of two dancers" $ do
      dance [Exchange 3 4] (dancers "eabcd") `shouldBe` dancers "eabdc"
    it "swaps the position of two dancers" $ do
      dance [Exchange 0 4] (dancers "abcde") `shouldBe` dancers "ebcda"
  describe "Partner" $ do
    it "swaps the position of two dancers by name" $ do
      dance [Partner 'e' 'b'] (dancers "eabdc") `shouldBe` dancers "baedc"

dance :: Dance -> Dancefloor -> Dancefloor
dance steps initialState = runSTUArray $ do
  dancers   <- MA.thaw initialState
  positions <- MA.thaw (invert initialState :: UArray Dancer Position)
  mapM_ (step dancers positions) steps
  return dancers
 where
   invert a = A.array (fromJust $ minmax $ A.elems a) [(v,i) | (i,v) <- A.assocs a]

-- we use two arrays, so that we don't have to do sequential scans
-- during Partner swaps. Spinning is always expensive, since it involves
-- a complete rewrite of both arrays.
step :: STUArray s Position Dancer -> STUArray s Dancer Position -> DanceMove -> ST s ()
step dancers positions = \case
  Spin n       -> do bs <- MA.getBounds dancers
                     rotate n bs
                     sequence_ [readArray dancers i >>= \d -> writeArray positions d i | i <- A.range bs]
  Exchange i j -> do a <- readArray dancers i
                     b <- readArray dancers j
                     swap i a j b
  Partner a b  -> do i <- readArray positions a
                     j <- readArray positions b
                     swap i a j b
 where
   rotate n bs = do
     ps <- sequence [(,) (spin bs n i) <$> (readArray dancers i) | i <- A.range bs]
     sequence_ [ writeArray dancers i v | (i,v) <- ps ]
   swap i a j b = do writeArray dancers i b
                     writeArray dancers j a
                     writeArray positions b i
                     writeArray positions a j

spin :: (Ix i, Num i, Integral i) => (i,i) -> i -> i -> i
spin bs n i = (i + n) `mod` fromIntegral (A.rangeSize bs)


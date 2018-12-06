{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE BangPatterns #-}

import Data.Array.ST
import Data.Ix
import Data.Array.MArray
import Control.Monad.ST

-- we are going to thrash the instructions
-- and we need random access, with bounds checking,
-- so an unpacked strict array is perfect
type Jumps s = STUArray s Int Int

main :: IO ()
main = do
  instructions <- fmap read . lines <$> getContents
  putStrLn "Simple inc:"
  print (runJumps succ instructions)
  putStrLn "Weird:"
  print (runJumps weird instructions)
    where
      weird offset | 3 <= offset = offset - 1
      weird offset               = offset + 1

-- intialise a list of instructions into an array of Jump instructions
jumps :: [Int] -> ST s (Jumps s)
jumps input = newListArray (0, length input - 1) input

runJumps :: (Int -> Int) -> [Int] -> Int
runJumps onJump input = runST $ do
  a     <- jumps input
  check <- inRange <$> getBounds a
  jump onJump check a

-- the jump loop
-- Takes a jump modifier, performed after each instruction
-- a bounds check (lifted out of the loop for efficiency)
-- the jump array itself
jump :: (Int -> Int) -> (Int -> Bool) -> Jumps s -> ST s Int
jump f check a = go 0 0
  where
    go !addr !n = do
            offset <- readArray a addr
            let nextAddr = addr + offset
                steps = n + 1
            if check nextAddr
               then writeArray a addr (f offset) >> go nextAddr steps
               else return steps

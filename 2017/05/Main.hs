import Data.Array.ST
import Data.Ix
import Data.Array.MArray
import Control.Monad.ST
import Control.Monad.State.Strict

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
runJumps onJump input = snd $ runST $ do
  a <- jumps input
  execStateT (jump onJump a) (0, 0)

-- the jump loop
-- Takes a jump modifier, performed after each instruction
-- the jump array itself
-- and the state of: (currentAddress, number of jumps performed)
jump :: (Int -> Int) -> Jumps s -> StateT (Int, Int) (ST s) ()
jump f a = go
  where go = do (addr, n) <- get
                v <- lift (readArray a addr)
                b <- lift (getBounds a)
                let addr' = addr + v
                if inRange b addr'
                   then do lift (writeArray a addr (f v))
                           put (addr', n + 1)
                           go
                   else return ()


{-# LANGUAGE DeriveGeneric #-}

import           Control.Monad
import qualified Data.Map.Strict     as M
import qualified Data.List           as L
import qualified Data.HashMap.Strict as HM
import qualified Data.Vector.Unboxed as V
import qualified Data.Vector.Generic.Mutable as MV
import           Data.Vector.Unboxed ((//))
import Data.Ord
import Data.Vector.Instances

-- import qualified Debug.Trace as Debug

type Blocks = Int -- doesn't need to be very big
type Banks = V.Vector Blocks

main :: IO ()
main = do
  bs <- V.fromList . fmap read . words <$> getContents
  print bs
  let (totalTime, cycleLen) = cycleLength bs
  putStrLn $ "Time until cycle: " ++ show totalTime
  putStrLn $ "Cycle length: " ++ show cycleLen

cycleLength :: Banks -> (Int, Int)
cycleLength bs = go 1 (HM.singleton bs 0) bs
  where
    go n seen b =
      let b' = shuffle b
       in case HM.lookup b' seen of
         Nothing   -> go (n + 1) (HM.insert b' n seen) b'
         Just step -> (HM.size seen, n - step)

shuffle :: Banks -> Banks
shuffle bs =
  let order (i,v) = (v, Down i)
      (i, b) = V.maximumBy (comparing order) (V.indexed bs)
      indices = fmap (`mod` V.length bs) [succ i .. ]
      allocations = M.toAscList
                  . M.fromListWith (+)
                  . take b
                  . zip indices
                  $ repeat 1
   in V.modify (addAll allocations) (bs // [(i, 0)])
  where
    addAll deltas v = forM_ deltas $ \(i,x) -> 
      MV.modify v (+ x) i

example :: Banks
example = V.fromList [0,2,7,0]

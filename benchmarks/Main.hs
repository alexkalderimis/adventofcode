import           Criterion
import           Criterion.Main (defaultMain)
import           Test.QuickCheck.Arbitrary
import           Test.QuickCheck.Gen

import           Elves.Coord
import qualified Elves.RTree               as RT
import           Elves.RTree               (RTree)

type Point = (Int,Int,Int)
type Tree = RTree Point ()

setupEnv :: IO (Point, Tree, Tree, Tree)
setupEnv = do
  p <- generate arbitrary
  small <- generate (vector 100)
  large <- generate (vector 500)
  xl    <- generate (vector 1000)
  let tree ps = RT.index $ zip ps (repeat ())
  return (p,tree small, tree large, tree xl)

main = defaultMain [
   env setupEnv $ \ ~(p,sm,lg,xl) -> bgroup "main"
     [ bgroup "straightLine"
       [ bgroup "sm"
         [ bench "nearestNeighbour" $ whnf (RT.nearestNeighbour straightLine p) sm
         , bench "nearestNeighbour2" $ whnf (RT.nearestNeighbour2 straightLine p) sm
         ]
       , bgroup "lg"
         [ bench "nearestNeighbour" $ whnf (RT.nearestNeighbour straightLine p) lg
         , bench "nearestNeighbour2" $ whnf (RT.nearestNeighbour2 straightLine p) lg
         ]
       , bgroup "xl"
         [ bench "nearestNeighbour" $ whnf (RT.nearestNeighbour straightLine p) xl
         , bench "nearestNeighbour2" $ whnf (RT.nearestNeighbour2 straightLine p) xl
         ]
       ]
     , bgroup "manhattan"
       [ bench "nearestNeighbour" $ whnf (RT.nearestNeighbour manhattan p) sm
       , bench "nearestNeighbour2" $ whnf (RT.nearestNeighbour2 manhattan p) sm
       ]
     ]
  ]

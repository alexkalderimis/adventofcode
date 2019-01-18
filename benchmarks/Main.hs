import           Criterion
import           Criterion.Main (defaultMain)
import           Test.QuickCheck.Arbitrary
import           Test.QuickCheck.Gen
import qualified Data.List as L
import           Control.Applicative
import           Data.Maybe

import           Elves.Coord
import qualified Elves.RTree               as RT
import           Elves.RTree               (RTree)

type Point = (Int,Int,Int)
type Tree = RTree Point ()

setupEnv :: IO (Point, Tree, Tree, Tree, [Point])
setupEnv = do
  p <- generate arbitrary
  small <- generate (vector 100)
  large <- generate (vector 500)
  xl    <- generate (vector 1000)
  let tree ps = RT.index $ zip ps (repeat ())
  return (p,tree small, tree large, tree xl, xl)

main = defaultMain [
   env setupEnv $ \ ~(p,sm,lg,xl,xls) -> bgroup "main"
     [ bgroup "straightLine"
       [ bgroup "sm"
         [ bench "straightLine" $ whnf (RT.nearestNeighbour straightLine p) sm
         , bench "manhattan"    $ whnf (RT.nearestNeighbour manhattan p) sm
         ]
       , bgroup "lg"
         [ bench "straightLine" $ whnf (RT.nearestNeighbour straightLine p) lg
         , bench "manhattan" $ whnf (RT.nearestNeighbour manhattan p) lg
         ]
       , bgroup "xl"
         [ bench "straightLine" $ whnf (RT.nearestNeighbour straightLine p) xl
         , bench "manhattan" $ whnf (RT.nearestNeighbour manhattan p) xl
         , bench "naive-baseline" $ whnf (naive p) xls
         ]
       ]
     ]
  ]

naive :: Point -> [Point] -> Maybe (Point)
naive p = fmap snd . L.foldl' go Nothing
  where
    go mx q = let dist = straightLine p q
               in case fromMaybe LT (compare (straightLine p q) . fst <$> mx) of
                    LT -> pure (dist, q)
                    _  -> mx

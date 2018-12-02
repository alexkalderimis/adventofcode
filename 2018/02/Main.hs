{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE BangPatterns #-}

import Data.Monoid
import Data.List

twosAndThrees :: String -> (Sum Int, Sum Int)
twosAndThrees input = (twos sorted, threes sorted)
  where
    sorted = sort input
    twos = go 2 0 Nothing
    threes = go 3 0 Nothing
    go target !n curr [] = if target == n then Sum 1 else Sum 0
    go target !n Nothing (x:xs) = go target (n + 1) (Just x) xs
    go target !n (Just x') (x:xs) =
      if | x      == x' -> go target (n + 1) (Just x) xs
         | target == n  -> Sum 1
         | otherwise    -> go target 1 (Just x) xs

diffsAtPosition as bs = getSum . mconcat $ zipWith f as bs
  where
    f a b | a == b = Sum 0
    f _ _          = Sum 1

common :: Eq a => [a] -> [a] -> [a]
common as bs = zip as bs >>= \(a,b) -> if a == b then [a] else []

pairs :: [a] -> [(a, a)]
pairs l = [(x,y) | (x:ys) <- tails l, y <- ys]

main :: IO ()
main = do
  strings <- lines <$> getContents
  let (twos,threes) = unzip . fmap twosAndThrees $ strings
      checksum = getSum (mconcat twos) * getSum (mconcat threes)
  putStrLn $ "Checksum: " <> show checksum
  let fingerprints = [common boxA boxB
                       | (boxA,boxB) <- pairs strings
                       , diffsAtPosition boxA boxB == 1
                       ]
  mapM_ print (nub . sort $ fingerprints)



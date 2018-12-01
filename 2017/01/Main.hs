import System.Environment
import Data.Monoid

capcha :: Int -> [Int] -> Int
capcha offset = sum . fmap f . pair
  where
    pair xs = zipWith (,) xs (drop offset $ cycle xs)
    f (a,b) = if a == b then a else 0

test :: IO ()
test = do
  [1,1,2,2] `is` 3
  [1,1,1,1] `is` 4
  [1,2,3,4] `is` 0
  [9,1,2,1,2,1,2,9] `is` 9
  where
    is input expected = putStrLn $ show input ++ " => " ++ show (capcha 1 input)
                                              ++ " (expected: " ++ show expected ++ ")"

main :: IO ()
main = do
  input <- fmap (read . return) <$> getLine 
  args <- getArgs
  let offset = case args of
        ["next"] -> 1
        ["mid"]  -> length input `div` 2
        []       -> 1
  print (capcha offset input)

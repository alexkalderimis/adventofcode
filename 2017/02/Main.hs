import Data.Foldable
import Data.Maybe
import Control.Applicative
import Data.List

checkRow :: [Int] -> Int
checkRow xs = let (small, big) = minmax xs
               in fromMaybe 0 ((-) <$> big <*> small)
  where
    minmax = foldl' mm (Nothing, Nothing)
    mm (sm, bg) x = ((min x <$> sm) <|> Just x, (max x <$> bg) <|> Just x)

pairs :: [a] -> [(a, a)]
pairs l = [(x,y) | (x:ys) <- tails l, y <- ys]

divisable a b = case (mod a b, mod b a) of
  (0, _) -> Just (a `div` b)
  (_, 0) -> Just (b `div` a)
  _      -> Nothing

divRow :: [Int] -> Int
divRow = fromMaybe 0 . listToMaybe . catMaybes . fmap (uncurry divisable) . pairs

main :: IO ()
main = do
  rows <- fmap (fmap read . words) . lines <$> getContents
  putStrLn $ "MINMAX: "    ++ (showCheck checkRow rows)
  putStrLn $ "DIVISIBLE: " ++ (showCheck divRow rows)
    where
      showCheck check = show . sum . fmap check

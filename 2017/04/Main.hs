import qualified Data.List as L
import qualified Data.Set as S

isValid :: (String -> String) -> String -> Bool
isValid f pwd = S.size s == length ws
  where
    ws = words pwd
    s = S.fromList (fmap f ws)

isValid01 :: String -> Bool
isValid01 = isValid id

isValid02 :: String -> Bool
isValid02 = isValid L.sort

main :: IO ()
main = do
  pwds <- lines <$> getContents
  putStrLn "No dups policy:"
  print (length $ filter isValid01 pwds)
  putStrLn "No anagrams policy:"
  print (length $ filter isValid02 pwds)

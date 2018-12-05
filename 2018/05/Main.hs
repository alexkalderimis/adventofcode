import Data.Char

main :: IO ()
main = do
  s <- getLine
  putStrLn "React of original"
  print . length $ react s
  let optimised = do
       c <- ['a' .. 'z']
       let candidate = filter (\c' -> c /= c' && toUpper c /= c') s
       let len = length (react candidate)
       return (c, len)
  putStrLn "Optimised length"
  mapM_ print optimised
  putStrLn . ("Minimum: " ++) . show . minimum . fmap snd $ optimised

-- react the string by collapsing sequences of 'reactive' characters (defined
-- as characters of same letter by opposite case). We traverse the string
-- maintaining a stack of past characters to enable back-tracking once we
-- delete a pair, as deletions can provoke further deletions.
react :: String -> String
react = go []
  where go past []       = reverse past
        go past (x:y:cs) | reactive x y = case past of []    -> go [] cs
                                                       (c:p) -> go p  (c:cs)
        go past (c:cs)   = go (c:past) cs

reactive :: Char -> Char -> Bool
reactive a b = (a /= b) && (toUpper a == b || toUpper b == a)

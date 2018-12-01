import Data.Monoid
import System.Environment
import Data.List (intercalate)
import qualified Data.IntSet as Set

main :: IO ()
main = do
  args <- getArgs
  changes <- fmap (fmap parse . lines) getContents 
  case args of
    ["sum"]          -> print (sum changes)
    ["first-repeat"] -> print (firstRepeated $ cycle changes)
    _  -> error $ "Bad args: " <> intercalate " " args
  where
    parse (sign:str) = let f = case sign of
                                '+' -> id
                                '-' -> negate
                                _   -> error $ "bad sign: " <> [sign]
                        in f $ read str

firstRepeated :: [Int] -> Maybe Int
firstRepeated = go (Set.singleton 0, 0)
  where
    go _ [] = Nothing
    go (seen, curr) (x:xs) =
         let next = curr + x
          in if Set.member next seen
                then Just next
                else go (Set.insert next seen, next) xs

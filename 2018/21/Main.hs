import Data.List
import Data.Bits
import Text.Printf
import qualified Data.IntSet as S

main :: IO ()
main = uncurry (printf "Last value in sequence: %d (seq. size: %d)\n")
               (lastBeforeLoop valuesOfE)

-- pt2 requires finding the last uniq value in the sequence.
--
-- I wasn't able to discern the pattern there, so just ran the
-- sequence until it looped.
--
lastBeforeLoop :: [Int] -> (Int, Int)
lastBeforeLoop (x:xs) = go x (S.singleton x) xs
  where
    go previous seen (x:xs)
      | S.member x seen = (previous, S.size seen)
      | otherwise       = go x (S.insert x seen) xs

-- the 'e' register in the programme represents a sequence,
-- produced using the following steps (decompiled manually
-- from reading the assembly).
--
-- should go 9079325, 1293036, 637585, 4545250, ...
valuesOfE :: [Int]
valuesOfE = unfoldr go 0
  where
    go e = let f   = e .|. 65536
               s   = loop' 1855046 f
            in Just (s, s)

    loop' e f = let e' = (((e + (f .&. 255)) .&. 16777215) * 65899) .&. 16777215
                 in if 256 > f then e'
                               else loop'' 256 0 e' f

    loop'' b c e f = if b > f
                     then loop' e c
                     else loop'' ((c + 2) * 256) (c + 1) e f
        

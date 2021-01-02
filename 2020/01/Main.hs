import Data.Hashable (Hashable)
import qualified Data.List as L
import qualified Data.Attoparsec.Text as A
import Text.Parser.Char (newline)
import Text.Parser.Combinators (sepBy1)
import qualified Data.HashMap.Strict as Map

import Elves
import Elves.Advent

main :: IO ()
main = day 01 parser pt1 pt2 test
  where
    parser = A.decimal `sepBy1` newline
    pt1 xs = case findPair xs of
      Nothing    -> putStrLn "No pair"
      Just (x,y) -> putStrLn $ show (x * y)
    pt2 xs = case findTriple xs of
      Nothing      -> putStrLn "No triple"
      Just (x,y,z) -> putStrLn $ show (x * y * z)

test = do
  let input = [ 1721 , 979 , 366 , 299 , 675 , 1456 ]
  describe "findPair" $ do
    specify "finds 1721 + 299 = 2020" $ do
      findPair input `shouldBe` Just (299, 1721)
    specify "finds nothing for an empty list" $ do
      findPair [] `shouldBe` Nothing
    specify "finds the first pair" $ do
      findPair [2000, 1900, 20, 120] `shouldBe` Just (20, 2000)

  describe "findTriple" $ do
    specify "finds 979 + 366 + 675 = 2020" $ do
      findTriple input `shouldBe` Just (979, 366, 675)
    specify "finds nothing for an empty list" $ do
      findTriple [] `shouldBe` Nothing

findPair :: [Int] -> Maybe (Int, Int)
findPair = first . find id

findTriple :: [Int] -> Maybe (Int, Int, Int)
findTriple xs =
  let f x = case x of Left ((_, x), (_, y)) -> x + y
                      Right (_, x) -> x
      indexed = zip [0..] xs
      xs' = (map Right indexed)
            ++
            (map Left [(a,b) | a <- indexed, b <- indexed, fst a /= fst b])

   in first
      . map (\(x,y,z) -> (snd x, snd y, snd z)) 
      . filter distinct
      $ (find f xs' >>= uncurry triples)

type Indexed a = (Int, a)
type OneOrTwo a = Either (a, a) a

distinct :: (Indexed a, Indexed b, Indexed c) -> Bool
distinct ((i, _), (j, _), (k, _)) = i /= j && i /= k && j /= k

triples :: OneOrTwo (Indexed Int) -> OneOrTwo (Indexed Int) -> [(Indexed Int, Indexed Int, Indexed Int)]
triples (Left (x,y)) (Right z) = pure (x,y,z)
triples (Right z) (Left (x,y)) = pure (x,y,z)
triples _ _ = []

first :: [a] -> Maybe a
first = fmap fst . L.uncons

find :: Hashable a => (a -> Int) -> [a] -> [(a, a)]
find f = L.unfoldr go . (,) Map.empty
  where
    go (m, xs) = do
      (x, xs) <- L.uncons xs
      let m' = Map.insert (2020 - f x) x m
      case Map.lookup (f x) m of
        Just y -> pure ((x, y), (m', xs))
        Nothing -> go (m', xs)

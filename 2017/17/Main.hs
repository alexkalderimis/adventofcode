import qualified Data.List    as L

import           Elves
import           Elves.Advent
import           Elves.CircularBuffer (Buffer)
import qualified Elves.CircularBuffer as B

main :: IO ()
main = staticDay 17 pt1 pt2 test
  where
    pt1 = print (nextVal $ runSpinLock 359 2017)
    pt2 = print (nextVal $ B.rewind $ runSpinLock 359 100000) -- 50000000)

test = do
  describe "example" $ do
    let b = B.singleton 0
        n = 3
        states = L.scanl (flip (insert n)) b [1 .. 2017]
    it "has the right foci" $ do
      (B.focus <$> take 10 states) `shouldBe` [0 .. 9]
    it "has the right indices" $ do
      (B.bufIdx <$> take 10 states) `shouldBe` [0,1,1,2,2,1,5,2,6,1]
    it "has the right states" $ do
      (B.toList <$> take 10 states)
        `shouldBe` [[0]
                   ,[0,1]
                   ,[0,2,1]
                   ,[0,2,3,1]
                   ,[0,2,4,3,1]
                   ,[0,5,2,4,3,1]
                   ,[0,5,2,4,3,6,1]
                   ,[0,5,7,2,4,3,6,1]
                   ,[0,5,7,2,4,3,8,6,1]
                   ,[0,9,5,7,2,4,3,8,6,1]
                   ]
    it "runs to the correct conclusion" $ do
      nextVal (runSpinLock n 2017) `shouldBe` 638

nextVal = B.focus . B.right

-- an interesting property holds: the next value right of 0 can be prediced from the sum
-- of the previous value and the number of times that value was seen right of 0:
--
-- λ> analysePt2 3 2017
-- [(0,1),(1,1),(2,3),(5,4),(9,3),(12,4),(16,202),(218,299),(517,709),(1226,792*)]
-- λ> analysePt2 359 2017
-- [(0,1),(1,1),(2,1),(3,1),(4,1),(5,1),(6,5),(11,1),(12,14),(26,142),(168,23),(191,70),(261,270),(531,1487*)]
--
-- Here the last values are marked with * to indicate that they are not complete, so cannot
-- be used to infer the next value.
--
-- If we can determine the sequence of lengths, then the full sequence can be predicted...
analysePt2 :: Int -> Int -> [(Int, Int)]
analysePt2 n j = fmap (head <#> length)
               . L.group
               . fmap (nextVal . B.rewind)
               $ L.scanl (flip (insert n)) (B.singleton 0) [1 .. j]

runSpinLock :: Int -> Int -> Buffer Int
runSpinLock n j = L.foldl' (flip (insert n)) (B.singleton 0) [1 .. j]

insert :: Int -> Int -> Buffer Int -> Buffer Int
insert n i = B.right . B.insertR i . B.shift n

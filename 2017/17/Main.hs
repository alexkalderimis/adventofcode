import qualified Data.List    as L

import           Elves
import           Elves.Advent
import           Elves.Zipper (Zipper)
import qualified Elves.Zipper as Z

type Buffer = Zipper Int

main :: IO ()
main = staticDay 17 pt1 pt2 test
  where
    pt1 = print (nextVal $ runSpinLock 359 2017)
    pt2 = print (nextVal $ Z.rewind $ runSpinLock 359 50000000)

test = do
  describe "example" $ do
    let b = Z.singleton 0
        n = 3
        states = L.scanl (flip (insert n)) b [1 .. 2017]
    it "has the right foci" $ do
      (Z.focus <$> take 10 states) `shouldBe` [0 .. 9]
    it "has the right indices" $ do
      (Z.idx <$> take 10 states) `shouldBe` [0,1,1,2,2,1,5,2,6,1]
    it "has the right states" $ do
      (Z.toList . Z.rewind <$> take 10 states)
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

nextVal = Z.focus . Z.right

runSpinLock :: Int -> Int -> Buffer
runSpinLock n j = L.foldl (flip (insert n)) (Z.singleton 0) [1 .. j]

insert :: Int -> Int -> Buffer -> Buffer
insert n i = Z.right . Z.insertR i . Z.shift n

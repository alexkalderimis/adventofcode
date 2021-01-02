import           Data.Foldable (foldl')
import qualified Data.Ix       as Ix
import           Data.Maybe
import           Data.Tree

import           Elves
import           Elves.Advent

main :: IO ()
main = staticDay 4 pt1 pt2 test
  where
    range = (402328,864247)
    candidates = dedup (takeDepth 6 =<< increasing 0 9)
    pt1 = print . length . filter (validPwd range) $ candidates
    pt2 = print . length . filter (validPwd2 range) $ candidates

test = do
  describe "hasDoublet" $ do
    let examples = [([1,1,2,2,3,3], True)
                   ,([1,2,3,4,4,4], False)
                   ,([1,1,2,2,2,2], True)
                   ,([1,1,1,2,2,2], False)
                   ,([1,1,1,1,2,2], True)
                   ,([], False)
                   ]
    forM_ examples $ \(pwd, expected) -> it (show pwd) $ do
      hasDoublet pwd `shouldBe` expected
  describe "validPwd" $ do
    let examples = [((0,30), [2,2], True)
                   ,((30, 40), [2,3], False)
                   ,((0,11), [1,1], True)
                   ,((0,111111), replicate 6 1, True)
                   ,((0,111111), replicate 6 0, True)
                   -- ,((0,999999), [2,2,3,4,5,0], False) -- we handle these by never considering them
                   ,((0,999999), [1,2,3,4,5,6], False)
                   ]
    forM_ examples $ \(rng, pwd, expected) -> it (show rng <> " " <> show pwd) $ do
      validPwd rng pwd `shouldBe` expected

  describe "toInt" $ do
    let examples = [([2,2], 22)
                   ,([2,3], 23)
                   ,([2,4], 24)
                   ,([2,5], 25)
                   ,([3,3], 33)
                   ,([3,0], 30)
                   ,([3,1,6,5,2,4], 316524)
                   ,([3,1,0,5,5,4], 310554)
                   ]
    forM_ examples $ \(digits, result) -> it ("handles " <> show digits) $ do
      toInt digits `shouldBe` result

  describe "11-55" $ do
    it "creates the correct candidate values" $ do
      let range = dedup (takeDepth 2 =<< increasing 1 5)
      range `shouldBe` [[1,1],[1,2],[1,3],[1,4],[1,5]
                       ,[2,2],[2,3],[2,4],[2,5]
                       ,[3,3],[3,4],[3,5]
                       ,[4,4],[4,5]
                       ,[5,5]
                       ]

validPwd rng pwd = Ix.inRange rng (toInt pwd) && hasRepeat pwd

validPwd2 rng pwd = Ix.inRange rng (toInt pwd) && hasDoublet pwd

hasRepeat (a:b:rst) = (a == b) || hasRepeat (b:rst)
hasRepeat _         = False

hasDoublet (a:b:rst) | a == b && listToMaybe rst /= Just a = True
hasDoublet (a:rst)   = hasDoublet (dropWhile (== a) rst)
hasDoublet _         = False

toInt :: [Int] -> Int
toInt = foldl' (\n x -> n * 10 + x) 0

increasing prev max = [Node x (increasing x max) | x <- [prev .. max]]

takeDepth 0 _           = [[]]
takeDepth n (Node x ts) = fmap (x:) (ts >>= takeDepth (n - 1))

dedup xs = case xs of { (a:b:rst) -> if a == b then dedup (b:rst) else a : dedup (b:rst); _ -> xs }


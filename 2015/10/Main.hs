import           Data.Attoparsec.Text (digit)
import qualified Data.Attoparsec.Text as A
import qualified Data.List            as L
import           Data.Text            (Text)
import qualified Data.Text            as T

import           Elves
import           Elves.Advent

-- there is probably a nice way to jump forward here, by detecting
-- cycles or the like. But this implementation is too fast to bother
main :: IO ()
main = day 10 parser pt1 pt2 test
  where
    parser = some (read . pure <$> digit)
    pt1 = print . length . applyN 40 lookSay
    pt2 = print . length . applyN 50 lookSay

test = do
  let examples = [([1]           ,[1,1])
                 ,([1,1]         ,[2,1])
                 ,([2,1]         ,[1,2,1,1])
                 ,([1,2,1,1]     ,[1,1,1,2,2,1])
                 ,([1,1,1,2,2,1] ,[3,1,2,2,1,1])
                 ]
  forM_ examples $ \(inp, expected) -> do
    describe (show inp) $ do
      it ("becomes " ++ show expected) $ do
        lookSay inp `shouldBe` expected

lookSay :: [Word] -> [Word]
lookSay ws = L.group ws >>= \grp ->
  case grp of []    -> []
              (x:_) -> [fromIntegral (length grp), x]

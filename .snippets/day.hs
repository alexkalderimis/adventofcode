import qualified Data.Text as T
import           Data.Text (Text)
import qualified Data.Attoparsec.Text as A

import Elves
import Elves.Advent

main :: IO ()
main = day  parser pt1 pt2 test
  where
    parser = A.takeText
    pt1 _ = error "unimplemented"
    pt2 _ = error "unimplemented"

test = pure ()

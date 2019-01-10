import Test.Hspec

import qualified ElvesSpec as ES
import qualified Elves.CliqueSpec as C
import qualified Elves.RTreeSpec as R

main :: IO ()
main = hspec $ foldr1 (>>) [ ES.spec, C.spec, R.spec ]

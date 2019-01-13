import Test.Hspec

import qualified ElvesSpec as ES
import qualified Elves.CliqueSpec as C
import qualified Elves.RTreeSpec as R
import qualified Elves.KnotSpec as K

main :: IO ()
main = hspec $ foldr1 (>>) [ ES.spec, C.spec, R.spec, K.spec ]

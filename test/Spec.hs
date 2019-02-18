import Test.Hspec

import qualified ElvesSpec as ES
import qualified Elves.CliqueSpec as C
import qualified Elves.RTreeSpec as R
import qualified Elves.KnotSpec as K
import qualified Elves.CoordSpec as Co
import qualified Elves.IntervalSpec as I
import qualified Elves.CountMapSpec as CM
import qualified Elves.Math.SymbolicSpec as MS

main :: IO ()
main = hspec $ foldr1 (>>) [ ES.spec, C.spec, R.spec, K.spec
                           , Co.spec, CM.spec, I.spec, MS.spec
                           ]

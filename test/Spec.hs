import Test.Hspec

import qualified ElvesSpec as ES
import qualified Elves.CliqueSpec as C
import qualified Elves.RTreeSpec as R
import qualified Elves.KnotSpec as K
import qualified Elves.CoordSpec as Co
import qualified Elves.TilingSpec as Tiling
import qualified Elves.IntervalSpec as I
import qualified Elves.CountMapSpec as CM
import qualified Elves.Math.SymbolicSpec as MSS
import qualified Elves.MatrixSpec as MS
import qualified Elves.Matrix.RotateSpec as MRS

main :: IO ()
main = hspec $ foldr1 (>>) [ ES.spec, C.spec, Tiling.spec, K.spec
                           , Co.spec, CM.spec, I.spec
                           , MS.spec, MRS.spec
                           , MSS.spec
                           , R.spec
                           ]

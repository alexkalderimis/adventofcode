import Test.HSpec

import qualified ElvesSpec as ES

main :: IO ()
main = hspec (describe "elves" ES.spec)

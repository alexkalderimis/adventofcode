{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

import qualified Data.Foldable as F
import qualified Data.List.Extra as L
import qualified Data.Text as T
import           Data.Text (Text)
import qualified Data.Attoparsec.Text as A
import           Control.Applicative.Combinators
import           Data.Attoparsec.Text            (digit)
import qualified Data.Set as Set
import           Data.Set (Set, (\\))

import qualified Data.Array.Unboxed as Array
import qualified Data.Array.MArray as MA
import           Data.Array.ST (runSTUArray, STUArray)
import qualified Control.Monad.ST as ST
import           Control.Monad.ST (ST)
import           Data.Array.Unboxed (UArray, Ix)

import Elves
import Elves.Advent
import Elves.StrictGrid

newtype Octopodes = Grid { octopodes :: UArray Coord Word } deriving (Eq)

instance Show Octopodes where
  show (Grid array) = let (a, b) = Array.bounds array
                          w = abs $ getCol (col b) - getCol (col a) + 1
                       in L.intercalate "\n"
                          . fmap mconcat
                          . L.chunksOf w
                          . fmap show
                          $ Array.elems array

-- working type
type OctopodesM s = STUArray s Coord Word

main :: IO ()
main = day 11 parser pt1 pt2 test
  where
    pt1 = print . fst . runCycles 100
    pt2 = print . findFirstFullFlash

test = do
  let parse = either fail pure . parseOnly parser . T.intercalate "\n"

  describe "small example" $ do
    let init = [ "11111"
               , "19991"
               , "19191"
               , "19991"
               , "11111"
               ]
        after_1 = [ "34543"
                  , "40004"
                  , "50005"
                  , "40004"
                  , "34543"
                  ]
        after_2 = [ "45654"
                  , "51115"
                  , "61116"
                  , "51115"
                  , "45654"
                  ]
    it "can model one step" $ do
      grid <- parse init
      grod <- parse after_1
      runCycles 1 grid `shouldBe` (9, grod)

    it "can model two steps" $ do
      grid <- parse init
      grod <- parse after_2
      runCycles 2 grid `shouldBe` (9, grod)

  describe "larger example" $ do
    let states = [(1, [ "6594254334"
                      , "3856965822"
                      , "6375667284"
                      , "7252447257"
                      , "7468496589"
                      , "5278635756"
                      , "3287952832"
                      , "7993992245"
                      , "5957959665"
                      , "6394862637"
                      ])
                 ,(2, [ "8807476555"
                      , "5089087054"
                      , "8597889608"
                      , "8485769600"
                      , "8700908800"
                      , "6600088989"
                      , "6800005943"
                      , "0000007456"
                      , "9000000876"
                      , "8700006848"
                      ])
                 ,(3, [ "0050900866"
                      , "8500800575"
                      , "9900000039"
                      , "9700000041"
                      , "9935080063"
                      , "7712300000"
                      , "7911250009"
                      , "2211130000"
                      , "0421125000"
                      , "0021119000"
                      ])
                 ,(4, [ "2263031977"
                      , "0923031697"
                      , "0032221150"
                      , "0041111163"
                      , "0076191174"
                      , "0053411122"
                      , "0042361120"
                      , "5532241122"
                      , "1532247211"
                      , "1132230211"
                      ])
                 ,(5, [ "4484144000"
                      , "2044144000"
                      , "2253333493"
                      , "1152333274"
                      , "1187303285"
                      , "1164633233"
                      , "1153472231"
                      , "6643352233"
                      , "2643358322"
                      , "2243341322"
                      ])
                 ,(6, [ "5595255111"
                      , "3155255222"
                      , "3364444605"
                      , "2263444496"
                      , "2298414396"
                      , "2275744344"
                      , "2264583342"
                      , "7754463344"
                      , "3754469433"
                      , "3354452433"
                      ])
                 ,(7, [ "6707366222"
                      , "4377366333"
                      , "4475555827"
                      , "3496655709"
                      , "3500625609"
                      , "3509955566"
                      , "3486694453"
                      , "8865585555"
                      , "4865580644"
                      , "4465574644"
                      ])
                 ,(8, [ "7818477333"
                      , "5488477444"
                      , "5697666949"
                      , "4608766830"
                      , "4734946730"
                      , "4740097688"
                      , "6900007564"
                      , "0000009666"
                      , "8000004755"
                      , "6800007755"
                      ])
                 ,(9, [ "9060000644"
                      , "7800000976"
                      , "6900000080"
                      , "5840000082"
                      , "5858000093"
                      , "6962400000"
                      , "8021250009"
                      , "2221130009"
                      , "9111128097"
                      , "7911119976"
                      ])
                 ,(10, [ "0481112976"
                       , "0031112009"
                       , "0041112504"
                       , "0081111406"
                       , "0099111306"
                       , "0093511233"
                       , "0442361130"
                       , "5532252350"
                       , "0532250600"
                       , "0032240000"
                       ])
                 ]
    it "can model 0 steps" $ do
      grid <- parse exampleInput
      runCycles 0 grid `shouldBe` (0, grid)

    forM_ states $ \(n, endState) -> do
      it ("can model " <> show n <> " steps") $ do
        grid <- parse exampleInput
        grod <- parse endState
        snd (runCycles n grid) `shouldBe` grod

    it "can model 100 steps" $ do
      grid <- parse exampleInput
      fst (runCycles 100 grid) `shouldBe` 1656

    it "can find the firt full flash" $ do
      grid <- parse exampleInput
      findFirstFullFlash grid `shouldBe` 195

parser :: Parser Octopodes
parser = Grid <$> gridP (read . pure <$> digit)

exampleInput :: [Text]
exampleInput = ["5483143223"
               ,"2745854711"
               ,"5264556173"
               ,"6141336146"
               ,"6357385478"
               ,"4167524645"
               ,"2176841721"
               ,"6882881134"
               ,"4846848554"
               ,"5283751526"
               ]

runCycles :: Word -> Octopodes -> (Int, Octopodes)
runCycles n (Grid iarr) = ST.runST $ do
  marr <- MA.thaw iarr
  n <- go marr 0 0
  iarr' <- MA.freeze marr
  pure (n, Grid iarr')
  where
    go marr m count = if m == n
                      then pure count
                      else step marr >>= \count' -> go marr (m + 1) (count + count')

findFirstFullFlash :: Octopodes -> Int
findFirstFullFlash (Grid iarr) = ST.runST (MA.thaw iarr >>= go 1)
  where
    total = Array.rangeSize (Array.bounds iarr)
    go n array = do count <- step array
                    if count == total
                       then pure n
                       else go (n + 1) array

step :: OctopodesM s -> ST s Int
step array = do
  bumpAll array
  flashers <- flash array
  forM_ flashers $ \i -> MA.writeArray array i 0
  pure (Set.size flashers)

bumpAll :: OctopodesM s -> ST s ()
bumpAll array = MA.getBounds array >>= bumpOctopodes array . Array.range

bumpOctopodes :: Foldable f => OctopodesM s -> f Coord -> ST s ()
bumpOctopodes array coords = forM_ coords $ \i -> do
  MA.readArray array i >>= MA.writeArray array i . succ

flash :: forall s. OctopodesM s -> ST s (Set Coord)
flash array = MA.getBounds array >>= go Set.empty
  where
    flashers flashed = fmap fst . filter (\(i, x) -> x > 9 && Set.notMember i flashed)

    go :: Set Coord -> (Coord, Coord) -> ST s (Set Coord)
    go flashed bs = do assocs <- MA.getAssocs array
                       case flashers flashed assocs of
                         [] -> pure flashed
                         newFlashers -> do
                           -- a neighbour can flash multiple times, so it is
                           -- important not to use a set here.
                           let neighbours = newFlashers >>= nextCoords True bs
                           bumpOctopodes array . filter (`Set.notMember` flashed)
                                               $ neighbours
                           go (flashed <> Set.fromList newFlashers) bs

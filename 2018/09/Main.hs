{-# LANGUAGE BangPatterns #-}

import System.Environment (withArgs, getArgs)

import Test.Hspec
import Control.Monad
import Text.Printf
import Data.Foldable
import qualified Data.IntMap as M

type Marble = Int
data Circle = Circle { marbles :: ![Int], currentIndex :: !Int }
  deriving (Show, Eq)

data GameState = GS { gameScores :: !(M.IntMap Int)
                    , gameCircle :: !Circle
                    } deriving (Show)

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["test"] -> withArgs [] (hspec spec)
    ["pt1"]  -> print (highScore 465 71940)
    ["pt2"]  -> print (highScore 465 (71940 * 100))
    _        -> putStrLn "missing argument: test, pt1, pt2"

currentMarble :: Circle -> Int
currentMarble c = marbles c !! currentIndex c

place :: Int -> Circle -> (Int, Circle)
-- rule two, scoring plays when marble is mod 23
place m (Circle ms i) | m `mod` 23 == 0 =
  let remIx = (i - 7) `mod` (length ms)
      taken = ms !! remIx
      (pref, suff) = splitAt remIx ms
   in (m + taken, Circle (pref ++ drop 1 suff) remIx)
-- rule one, default placement rule
place m (Circle ms i) =
  let j = (i + 2) `mod` length ms
      circle = case j of
                 0 -> Circle (ms ++ [m]) (length ms)
                 _ -> let (pref,suff) = splitAt j  ms
                       in Circle (pref ++ m : suff) j
   in (0, circle)

initCircle :: Circle
initCircle = Circle [0] 0

-- infinite list used to initialise games
players :: [(Int, Int)]
players = zip [0 ..] (repeat 0)

highScore :: Int -> Int -> Int
highScore numPlayers lastMarble = do
  let -- scores = M.fromList $ take numPlayers players
      ms = [1 .. lastMarble]
      initState = ((0,0), initCircle)
      finalScores = M.fromListWith (+)
                  $ fmap fst
                  $ scanl step initState ms
   in maximum $ M.elems finalScores
  where
    step (_, c) m =
      let player = m `mod` numPlayers
          ~(score, c') = place m c
       in ((player, score), c')

-- IX   0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20 21 22 23
----------------------------------------------------------------------------------
-- [-] (0)
-- [1]  0 (1)
-- [2]  0 (2) 1 
-- [3]  0  2  1 (3)
-- [4]  0 (4) 2  1  3 
-- [5]  0  4  2 (5) 1  3 
-- [6]  0  4  2  5  1 (6) 3 
-- [7]  0  4  2  5  1  6  3 (7)
-- [8]  0 (8) 4  2  5  1  6  3  7 
-- [9]  0  8  4 (9) 2  5  1  6  3  7 
-- [1]  0  8  4  9  2(10) 5  1  6  3  7 
-- [2]  0  8  4  9  2 10  5(11) 1  6  3  7 
-- [3]  0  8  4  9  2 10  5 11  1(12) 6  3  7 
-- [4]  0  8  4  9  2 10  5 11  1 12  6(13) 3  7 
-- [5]  0  8  4  9  2 10  5 11  1 12  6 13  3(14) 7 
-- [6]  0  8  4  9  2 10  5 11  1 12  6 13  3 14  7(15)
-- [7]  0(16) 8  4  9  2 10  5 11  1 12  6 13  3 14  7 15 
-- [8]  0 16  8(17) 4  9  2 10  5 11  1 12  6 13  3 14  7 15 
-- [9]  0 16  8 17  4(18) 9  2 10  5 11  1 12  6 13  3 14  7 15 
-- [1]  0 16  8 17  4 18  9(19) 2 10  5 11  1 12  6 13  3 14  7 15 
-- [2]  0 16  8 17  4 18  9 19  2(20)10  5 11  1 12  6 13  3 14  7 15 
-- [3]  0 16  8 17  4 18  9 19  2 20 10(21) 5 11  1 12  6 13  3 14  7 15 
-- [4]  0 16  8 17  4 18  9 19  2 20 10 21  5(22)11  1 12  6 13  3 14  7 15 
-- [5]  0 16  8 17  4 18(19) 2 20 10 21  5 22 11  1 12  6 13  3 14  7 15 
-- [6]  0 16  8 17  4 18 19  2(24)20 10 21  5 22 11  1 12  6 13  3 14  7 15 
-- [7]  0 16  8 17  4 18 19  2 24 20(25)10 21  5 22 11  1 12  6 13  3 14  7 15
----------------------------------------------------------------------------------
-- IX   0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20 21 22 23
spec :: Spec
spec = do
  --10 players; last marble is worth 1618 points: high score is 8317
  --13 players; last marble is worth 7999 points: high score is 146373
  --17 players; last marble is worth 1104 points: high score is 2764
  --21 players; last marble is worth 6111 points: high score is 54718
  --30 players; last marble is worth 5807 points: high score is 37305
  describe "example high-scores" $ do
    let table = [(7, 25, 32)
                ,(10, 1618, 8317)
                ,(13, 7999, 146373)
                ,(17, 1104, 2764)
                ,(21, 6111, 54718)
                ,(30, 5807, 37305)
                ]
    forM_ table $ \(players, lm, hs) -> do
      describe (printf "%d players, last marble %d" players lm) $ do
        it (printf "should score %d" hs) $ do
          highScore players lm `shouldBe` hs
  describe "placing the next marble" $ do
    describe "into the initial game state" $ do
      it "is the same as appending" $ do
        place 1 initCircle `shouldBe` (0, Circle [0,1] 1)
    describe "into step 1" $ do
      it "inserts between the two current values" $ do
        let c = (Circle [0,1] 1)
        place 2 c `shouldBe` (0, Circle [0,2,1] 1)
    describe "into step 2" $ do
      it "appends onto end" $ do
        let c = (Circle [0,2,1] 1)
        place 3 c `shouldBe` (0, Circle [0,2,1,3] 3)
  describe "the example game" $ do
    let play (_, c) m = place m c
        game = scanl play (0, initCircle) [1 .. 25]
        endState = last game
    it "should end up with a circle of 23 marbles" $ do
      length (marbles $ snd endState) `shouldBe` 24
    it "should play as expected" $ do
      let expected = [ 0, 1,   1,  3,  1,  3, 5, 7, 1, 3, 5
                     , 7,   9, 11, 13, 15, 1, 3, 5, 7, 9
                     , 11, 13,  6,  8, 10
                     ]
      fmap (currentIndex . snd) game `shouldBe` expected
    it "should include one scoring move" $ do
      let scores = filter (> 0) $ fmap fst game
      scores `shouldBe` [32]
      

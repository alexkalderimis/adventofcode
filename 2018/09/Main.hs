{-# LANGUAGE FlexibleContexts #-}

-- This exercise is all about efficiency, as being able to return answers
-- quickly is only possible with efficient representations.
--
-- The most efficient representation for this exercise is a circular linked
-- list. The closest thing we can get in Haskell while maintaining a persistent
-- data structure is a list-zipper.
--
-- Note that while the instructions talk a lot about indices, they are not
-- needed at all to represent the solution, so they are not present here at all

import System.Environment (withArgs, getArgs)

import Control.Monad
import Text.Printf
import Data.Foldable
import qualified Data.Vector.Unboxed as V
import qualified Data.Time.Clock as Clock

import Data.Array.ST
import Control.Monad.ST
import qualified Data.Array as A

import Elves.Advent hiding (focus)

type Marble = Int

-- A circle has at least one element (the focus), and 
-- a reversed stack to the left, and a stack to the right.
data Circle = Circle
  { lStack :: ![Marble]
  , rStack :: ![Marble]
  , focus  :: !Marble
  } deriving (Show, Eq)

marbles :: Circle -> [Marble]
marbles c = reverse (lStack c) <> (focus c : rStack c)

main :: IO ()
main = staticDay 9 pt1 pt2 spec
  where
    pt1 = print (highScore 465 71940)
    pt2 = print (highScore 465 (71940 * 100))

currentMarble :: Circle -> Int
currentMarble = focus

place :: Int -> Circle -> (Int, Circle)
place m c
  | m `mod` 23 == 0 = let (taken, circle) = pop (rotLeft 7 c)
                       in (m + taken, circle)
  | otherwise       = let (Circle l r foc) = rotRight 1 c
                       in (0, Circle (foc:l) r m)

showCircle :: Circle -> String
showCircle (Circle ls rs foc) =
  unwords $ concat [fmap show (reverse ls)
                   ,["(" <> show foc <> ")"]
                   ,fmap show rs
                   ]

initCircle :: Circle
initCircle = Circle [] [] 0

-- Circle navigation functions:
-- rotating left and right, and popping the current element
rotRight :: Int -> Circle -> Circle
rotRight 0 c = c
rotRight n (Circle [] [] foc) = Circle [] [] foc
rotRight n (Circle ls rs foc) =
  let ls'      = foc : ls
      rotated  = case rs of (x:xs) -> Circle ls' xs x
                            [] -> let (x:xs) = reverse ls'
                                  in Circle [] xs x
   in rotRight (n - 1) rotated

rotLeft :: Int -> Circle -> Circle
rotLeft 0 c = c
rotLeft n (Circle [] [] foc) = Circle [] [] foc
rotLeft n (Circle ls rs foc) =
  let rs'      = foc : rs
      rotated  = case ls of (x:xs) -> Circle xs rs' x
                            [] -> let (x:xs) = reverse rs'
                                  in Circle xs [] x
   in rotLeft (n - 1) rotated

{-# INLINE pop #-}
pop :: Circle -> (Marble, Circle)
pop (Circle [] [] _) = error "Cannot pop singleton circle"
pop (Circle ls rs foc) = 
  let circle = case rs of
                 (x:xs) -> Circle ls xs x
                 [] -> let (x:xs) = reverse ls in Circle [] xs x
   in (foc, circle)

-- a relic of an attempt to calculate the answer. Completely
-- hopeless, but left in here as a high level check
lengthAt :: Marble -> Int
lengthAt 0 = 1
lengthAt n = lengthAt 0 + n - (2 * (n `div` 23))

-- Place the marbles, calculate the scores, and then
-- use an unpacked ST array to find the highest (this
-- is a small optimisation, but a significant one)
highScore :: Int -> Int -> Int
highScore numPlayers lastMarble = do
  let 
      ms          = [1 .. lastMarble]
      initState   = ((0,0), initCircle)
      finalScores = buildScores (fst <$> scanl step initState ms)
   in maximum $ A.elems finalScores
  where
    buildScores scores = runST $ do
      a <- newArray (0, numPlayers - 1) 0
      mapM_ (addScore a) scores
      freeze a
    addScore :: STUArray s Int Int -> (Int, Int) -> ST s ()
    addScore a (k,v) = do
      curr <- readArray a k
      writeArray a k (curr + v)
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
      let (_, c) = place 1 initCircle
      it "is the same as appending" $ do
        marbles c `shouldBe` [0, 1] 
      it "has the current marble as the focus" $ do
        focus c `shouldBe` 1
    describe "into step 1" $ do
      let c = snd $ foldl (\(_,c) m -> place m c) (0, initCircle) [1,2]
      it "inserts between the two current values" $ do
        marbles c `shouldBe` [0,2,1] 
  describe "lengthAt" $ do
    it "should correctly calculate the length at each game state" $ do
    let play (_, c) m = place m c
        game = scanl play (0, initCircle) [1 .. 25]
        lengths = fmap (length . marbles . snd) game
    fmap lengthAt [0 .. 25] `shouldBe` lengths
  describe "the example game" $ do
    let play (_, c) m = place m c
        game = scanl play (0, initCircle) [1 .. 25]
        endState = last game
    it "should end up with a circle of 23 marbles" $ do
      length (marbles $ snd endState) `shouldBe` 24
    it "should include one scoring move" $ do
      let scores = filter (> 0) $ fmap fst game
      scores `shouldBe` [32]

{-# LANGUAGE OverloadedStrings, GeneralizedNewtypeDeriving #-}

import Safe (atMay, headMay)
import Data.Ord
import qualified Data.List as L
import qualified Data.Text as T
import           Data.Text (Text)
import qualified Data.Attoparsec.Text as A
import Text.Parser.Combinators (sepBy1, choice, count)
import Text.Parser.Char (newline, space, text)

import qualified Data.Ix as Ix
import           Data.Ix (Ix)
import Data.IntSet (IntSet)
import qualified Data.IntSet as IntSet

import Elves
import Elves.Advent

newtype Number = Number { fromNumber :: Int } deriving (Show, Eq, Ord, Ix)
data RNG = RNG [Number] deriving (Show, Eq)

data Board = Board
  { range :: (Number, Number)
  , sets :: [IntSet]
  , marked :: [Number]
  }
  deriving (Show, Eq)

data PuzzleInput = PI { boards :: [Board], rng :: RNG }

main :: IO ()
main = day 04 inputP pt1 pt2 test
  where
    pt1 i = case winningBoard i of
              Nothing -> putStrLn "No winner!"
              Just b -> print (score b)
    pt2 i = case losingBoard i of
              Nothing -> putStrLn "No winner!"
              Just b -> print (score b)

inputP :: Parser PuzzleInput
inputP = do
  nums <- number `sepBy1` text ","
  some newline
  boards <- boardP `sepBy1` newline
  pure (PI { boards = boards, rng = RNG nums })

boardP :: Parser Board
boardP = do
  rows <- count 5 row
  let lo = minimum (mconcat rows)
      hi = maximum (mconcat rows)
      cols = L.transpose rows
      sets = fmap (IntSet.fromList . fmap fromNumber) (rows <> cols)

  pure $ Board { range = (lo, hi), marked = [], sets = sets }
  where 
      row = count 5 (many space *> number) <* newline

winningBoard' :: (PuzzleInput -> Maybe a) -> PuzzleInput -> Maybe a
winningBoard' f pi
  | null (boards pi) = Nothing
  | exhausted (rng pi) = Nothing
  | otherwise = f pi <|> winningBoard' f (markBoards pi)

losingBoard :: PuzzleInput -> Maybe Board
losingBoard pi
  | null (boards pi) = Nothing
  | exhausted (rng pi) = let mostMarked = Down . length . marked
                          in headMay
                             . L.sortBy (\a b -> compare (mostMarked a) (mostMarked b))
                             . filter complete
                             $ boards pi
                          
  | otherwise = losingBoard (markBoards pi)

markBoards :: PuzzleInput -> PuzzleInput
markBoards pi = let RNG (n:ns) = rng pi
                 in pi { rng = RNG ns, boards = fmap (mark n) (boards pi) }

winningBoardIndex = winningBoard' (L.findIndex complete . boards)
winningBoard = winningBoard' (L.find complete . boards)

score :: Board -> Int
score b = let winningNumber = head (marked b)
              unmarked = take 5 (sets b) >>= IntSet.elems
           in fromNumber winningNumber * sum unmarked

exhausted :: RNG -> Bool
exhausted (RNG []) = True
exhausted _ = False

includes :: Number -> Board -> Bool
includes n b = Ix.inRange (range b) n

removeCompleted :: PuzzleInput -> PuzzleInput
removeCompleted pi = pi { boards = filter (not . complete) (boards pi) }

mark :: Number -> Board -> Board
mark n b
  | complete b = b
  | not (includes n b) = b
  | otherwise = b { marked = n : marked b
                  , sets = fmap (IntSet.delete (fromNumber n)) (sets b)
                  }

number = fmap Number A.decimal

complete :: Board -> Bool
complete = any IntSet.null . sets

test = beforeAll loadInput $ do
  it "can find the winner" $ \testInput -> do
    winningBoardIndex testInput `shouldBe` Just 2
  it "gets the score of the winning board correct" $ \testInput -> do
    fmap score (winningBoard testInput) `shouldBe` Just 4512
  it "gets the score of the losing board correct" $ \testInput -> do
    fmap score (losingBoard testInput) `shouldBe` Just 1924

  where
    loadInput = do
      ti <- (T.pack <$> readFile "2021/04/test-input")
      case parseOnly inputP ti of
        Right testInput -> pure testInput
        Left msg -> error msg

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}

import           Control.Applicative.Combinators
import           Control.Monad
import           Control.Monad.ST
import           Data.Attoparsec.Text            (decimal, char)
import           Elves
import Data.Word
import           Elves.Advent
import           Text.Parser.Char                (newline)
import           Data.Array.ST           (STUArray, runSTUArray)
import           Data.Array.Unboxed      (UArray)
import qualified Data.Array.Unboxed      as A
import           Data.Array              (Ix)
import           Data.Array.MArray       (readArray, writeArray)
import qualified Data.Array.MArray       as MA
import Test.QuickCheck (property)

type Position = Word8
type Value = Int
type Program = UArray Position Value
type Heap s = STUArray s Position Value

data Instruction = Exit
                 | Add Position Position Position Position
                 | Mul Position Position Position Position
                 deriving (Show, Eq)

main :: IO ()
main = day 1 parse pt1 pt2 test
  where
    pt1 = print . (A.! 0) . fixProgram
    pt2 _ = print ()
    parse = fmap readProgram (decimal `sepBy1` char ',')

fixProgram :: Program -> Program
fixProgram p = runST (calculate $ restoreTo1202 p)
    where restoreTo1202 = (A.// [(1, 12), (2, 2)])

calculate :: Program -> ST s Program
calculate prog = do
  heap <- MA.thaw prog
  init <- nextInstruction 0 heap
  let go minstr = case minstr of
                    Nothing -> MA.freeze heap
                    Just instr -> applyInstruction instr heap >>= go
  go init

readProgram :: [Value] -> Program
readProgram vals = A.listArray (0, fromIntegral (length vals) - 1) vals

applyInstruction :: Instruction -> Heap s -> ST s (Maybe Instruction)
applyInstruction instr h = case instr of
  Exit -> pure Nothing
  (Add a b r k) -> go (+) a b r k
  (Mul a b r k) -> go (*) a b r k
  where
    go f a b r k = do
      bs <- MA.getBounds h
      mx <- tryRead h bs a
      my <- tryRead h bs b
      case liftA2 f mx my of
        Nothing -> pure Nothing
        Just res -> do MA.writeArray h r res
                       nextInstruction k h

nextInstruction :: Position -> Heap s -> ST s (Maybe Instruction)
nextInstruction index heap = do
  bs <- MA.getBounds heap
  code <- get bs index
  case code of
    Just 99 -> pure (Just Exit)
    Just  1 -> instr bs Add
    Just  2 -> instr bs Mul
    _       -> pure Nothing
  where
    get = tryRead heap
    instr bs f = liftM4 f <$> (fmap fromIntegral <$> get bs (index + 1))
                          <*> (fmap fromIntegral <$> get bs (index + 2))
                          <*> (fmap fromIntegral <$> get bs (index + 3))
                          <*> pure (pure $ index + 4)

tryRead :: Heap s -> (Position, Position) -> Position -> ST s (Maybe Value)
tryRead heap bs i = if MA.inRange bs i
                    then Just <$> readArray heap i
                    else pure Nothing

test = do
  describe "applyInstruction" $ do
    let examples =
         [ ([1,0,0,0,99], Add 0 0 0 4, [2,0,0,0,99])
         , ([1,0,0,0,99], Exit, [1,0,0,0,99])
         , ([2,3,0,3,99], Mul 3 0 3 4, [2,3,0,6,99])
         , ([1,9,10,3,2,3,11,0,99,30,40,50], Mul 3 11 0 8, [150,9,10,3,2,3,11,0,99,30,40,50])
         ]
    forM_ examples $ \(state, command, expected) -> do
      it ("gets the right result for: applyInstruction " <> show command <> " " <> show state) $ do
        let prog = readProgram state
            result = runST $ do heap <- MA.thaw prog
                                applyInstruction command heap
                                MA.freeze heap
        result `shouldBe` readProgram expected
    it "always leaves a program untouched when we exit" . property $ \ints ->
      let prog = readProgram ints
          result = runST $ do heap <- MA.thaw prog
                              applyInstruction Exit heap
                              MA.freeze heap
       in result == prog
      
  describe "nextInstruction" $ do
    let examples = [ (0, [1,0,0,0,99], Just (Add 0 0 0 4))
                   , (4, [1,0,0,0,99], Just Exit)
                   , (0, [2,3,0,3,99], Just (Mul 3 0 3 4))
                   , (4, [1,9,10,3,2,3,11,0,99,30,40,50], Just (Mul 3 11 0 8))
                   , (8, [1,9,10,3,2,3,11,0,99,30,40,50], Just Exit)
                   , (9, [1,9,10,3,2,3,11,0,99,30,40,50], Nothing)
                   ]
    forM_ examples $ \(pos, state, result) -> do
      it ("gets the right result for: nextInstruction " <> show pos <> " " <> show state) $ do
        let instruction = runST $ do
              heap <- MA.thaw (readProgram state)
              nextInstruction pos heap
        instruction `shouldBe` result

  describe "calculate" $ do
    let examples = [ ([1,0,0,0,99], [2,0,0,0,99])
                   , ([2,3,0,3,99], [2,3,0,6,99])
                   , ([2,4,4,5,99,0], [2,4,4,5,99,9801])
                   , ([1,1,1,4,99,5,6,0,99], [30,1,1,4,2,5,6,0,99])
                   , ([1,9,10,3,2,3,11,0,99,30,40,50]
                     ,[3500,9,10,70, 2,3,11,0, 99, 30,40,50]
                     )
                   ]
    forM_ (zip [0 ..] examples) $ \(i, (initialState, finalState)) -> do
      it ("gets the right result for program " <> show i) $ do
        let program = readProgram initialState
            result = runST (calculate $ readProgram initialState)
        A.elems result `shouldBe` finalState

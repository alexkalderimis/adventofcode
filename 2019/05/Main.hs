{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE TupleSections   #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RankNTypes         #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE UndecidableInstances        #-}

import           Control.Applicative.Combinators
import           Control.Monad
import           Control.Monad.ST
import           Data.Array                      (Ix)
import           Data.Array.MArray               (readArray, writeArray)
import qualified Data.Array.MArray               as MA
import           Data.Array.ST                   (STUArray, runSTUArray)
import           Data.Array.Unboxed              (UArray)
import qualified Data.Array.Unboxed              as A
import           Data.Attoparsec.Text            (char, decimal)
import Data.Functor.Identity
import           Data.List                       (minimumBy)
import           Data.Maybe
import           Data.Ord                        (comparing)
import           Data.Word
import           Elves
import           Elves.Advent
import           Elves.Coord                     (Accessor, dimensions,
                                                  getDimension, midpoint,
                                                  origin, scale, setDimension,
                                                  translate)
import           Test.QuickCheck                 (property)
import           Text.Parser.Char                (newline)

import           Debug.Trace                     (traceShow)

type Addr = Word8
type Value = Int
type Program = UArray Addr Value
type Heap s = STUArray s Addr Value
type Stream = [Value]
data Parameter = Position Addr | Immediate Value
                 deriving (Show, Eq)

data Instruction = Exit
                 | Add Parameter Parameter Addr Addr
                 | Mul Parameter Parameter Addr Addr
                 | Input Addr Addr
                 | Output Parameter Addr
                 deriving (Show, Eq)

main :: IO ()
main = day 5 parse pt1 pt2 test
  where
    parse = fmap readProgram (decimal `sepBy1` char ',')
    pt1 = const (print ())
    pt2 = const (print ())

value0 :: Program -> Value
value0 = (A.! 0)

calculate :: Stream -> Program -> ST s (ExecutionState Maybe Program)
calculate input prog = do
  heap <- MA.thaw prog
  init <- nextInstruction 0 heap
  let go s  = case esKont s of
                    Nothing   -> do h <- MA.freeze heap
                                    pure s { esHeap = h }
                    Just inst -> applyInstruction s { esKont = Identity inst } >>= go
  go (newState heap input init)

readProgram :: [Value] -> Program
readProgram vals = A.listArray (0, fromIntegral (length vals) - 1) vals

data ExecutionState f h = ExecutionState
  { esKont :: f Instruction
  , esInput :: Stream
  , esOutput :: Stream
  , esErr :: [String]
  , esHeap :: h
  }

deriving instance (Show (f Instruction), Show h) => Show (ExecutionState f h)
deriving instance (Eq (f Instruction), Eq h) => Eq (ExecutionState f h)

newState :: h -> Stream -> f Instruction -> ExecutionState f h
newState h input i = ExecutionState { esKont = i
                                       , esInput = input
                                       , esOutput = mempty
                                       , esErr = mempty
                                       , esHeap = h
                                       }

applyInstruction :: ExecutionState Identity (Heap s) -> ST s (ExecutionState Maybe (Heap s))
applyInstruction state@ExecutionState{..} = case runIdentity esKont of
  Exit          -> pure state { esKont = Nothing }
  (Add a b r k) -> go (+) a b r k
  (Mul a b r k) -> go (*) a b r k
  (Output p k)  -> do bs <- MA.getBounds esHeap
                      mx <- resolve bs p
                      case mx of
                        Nothing -> pure referenceError
                        Just v  -> do i <- nextInstruction k esHeap
                                      pure state { esKont = i, esOutput = v : esOutput }
  (Input a k)   -> case esInput of
                      [] -> pure state { esKont = Nothing
                                       , esErr = "Unexpected End of Input" : esErr
                                       }
                      (x:xs) -> do MA.writeArray esHeap a x
                                   kont <- nextInstruction k esHeap
                                   pure state { esKont = kont, esInput = xs }
  where
    resolve _ (Immediate v)    = pure (pure v)
    resolve bs (Position addr) = tryRead esHeap bs addr
    referenceError = state { esKont = Nothing, esErr = "Reference error" : esErr }
    go f a b r k = do
      bs <- MA.getBounds esHeap
      mx <- resolve bs a
      my <- resolve bs b
      case liftA2 f mx my of
        Nothing -> pure referenceError
        Just res -> do MA.writeArray esHeap r res
                       kont <- nextInstruction k esHeap
                       pure state { esKont = kont }

nextInstruction :: Addr -> Heap s -> ST s (Maybe Instruction)
nextInstruction index heap = do
  bs <- MA.getBounds heap
  code <- get bs index
  case fmap (`quotRem` 100) code of
    Just (0, 99) -> pure (Just Exit)
    Just (ms,  1) -> instr2 (modes ms) bs Add
    Just (ms,  2) -> instr2 (modes ms) bs Mul
    Just (_,  3) -> liftM2 Input <$> (fmap fromIntegral <$> get bs (index + 1))
                                 <*> pure (pure $ index + 2)
    Just (ms,  4) -> let m = head (modes ms)
                      in liftM2 Output <$> (fmap m <$> get bs (index + 1))
                                       <*> pure (pure $ index + 2)
    _            -> pure Nothing
  where
    get = tryRead heap
    instr2 (ma:mb:_) bs f = liftM4 f <$> (fmap ma <$> get bs (index + 1))
                                     <*> (fmap mb <$> get bs (index + 2))
                                     <*> (fmap fromIntegral <$> get bs (index + 3))
                                     <*> pure (pure $ index + 4)
    modes n = let (m, code) = quotRem n 10
                  mode = case code of
                            0 -> Position . fromIntegral
                            1 -> Immediate
                            _ -> error ("Unknown parameter mode: " <> show code)
               in mode : modes m

tryRead :: Heap s -> (Addr, Addr) -> Addr -> ST s (Maybe Value)
tryRead heap bs i = if MA.inRange bs i
                    then Just <$> readArray heap i
                    else pure Nothing

test = do
  describe "applyInstruction" $ do
    let examples =
         [ ([1,0,0,0,99], Add (Position 0) (Position 0) 0 4, [2,0,0,0,99])
         , ([1,0,0,0,99], Exit, [1,0,0,0,99])
         , ([2,3,0,3,99], Mul (Position 3) (Position 0) 3 4, [2,3,0,6,99])
         , ([2,3,0,3,99], Input 1 4, [2,10000,0,3,99])
         , ([1,9,10,3,2,3,11,0,99,30,40,50], Mul (Position 3) (Position 11) 0 8, [150,9,10,3,2,3,11,0,99,30,40,50])
         ]
    forM_ examples $ \(state, command, expected) -> do
      it ("gets the right result for: applyInstruction " <> show command <> " " <> show state) $ do
        let prog = readProgram state
            result = runST $ do heap <- MA.thaw prog
                                applyInstruction (newState heap [10000 ..] (Identity command))
                                MA.freeze heap
        result `shouldBe` readProgram expected
    it "always leaves a program untouched when we exit" . property $ \ints ->
      let prog = readProgram ints
          result = runST $ do heap <- MA.thaw prog
                              applyInstruction (newState heap mempty (Identity Exit))
                              MA.freeze heap
       in result == prog
    it "handles positional output instructions" $ do
      let prog = readProgram (take 10 [1 ..])
          result = runST $ do
                     heap <- MA.thaw prog
                     s <- applyInstruction (newState heap mempty (Identity (Output (Position 5) 0)))
                     pure (esOutput s)
      result `shouldBe` [6]
    it "handles immediate output instructions" $ do
      let prog = readProgram (take 10 [1 ..])
          result = runST $ do
                     heap <- MA.thaw prog
                     s <- applyInstruction (newState heap mempty (Identity (Output (Immediate 5) 0)))
                     pure (esOutput s)
      result `shouldBe` [5]
    it "does not mutate the heap on output" . property $ \ints ->
      let prog = readProgram ints
          result = runST $ do heap <- MA.thaw prog
                              let s = newState heap mempty (Identity $ Output (Immediate 5) 0)
                              s' <- applyInstruction s
                              _  <- applyInstruction s { esKont = Identity (Output (Position 5) 0) }
                              MA.freeze heap
       in result == prog

  describe "nextInstruction" $ do
    let examples = [ (0, [1,0,0,0,99], Just (Add (Position 0) (Position 0) 0 4))
                   , (4, [1,0,0,0,99], Just Exit)
                   , (0, [2,3,0,3,99], Just (Mul (Position 3) (Position 0) 3 4))
                   , (0, [1002,3,0,3,99], Just (Mul (Position 3) (Immediate 0) 3 4))
                   , (0, [102,3,0,3,99], Just (Mul (Immediate 3) (Position 0) 3 4))
                   , (0, [101,3,0,3,99], Just (Add (Immediate 3) (Position 0) 3 4))
                   , (0, [1001,3,0,3,99], Just (Add (Position 3) (Immediate 0) 3 4))
                   , (4, [1,9,10,3,2,3,11,0,99,30,40,50], Just (Mul (Position 3) (Position 11) 0 8))
                   , (8, [1,9,10,3,2,3,11,0,99,30,40,50], Just Exit)
                   , (9, [1,9,10,3,2,3,11,0,99,30,40,50], Nothing)
                   , (0, [3,3,0,3,99], Just (Input 3 2))
                   , (0, [4,3,0,3,99], Just (Output (Position 3) 2))
                   , (0, [104,3,0,3,99], Just (Output (Immediate 3) 2))
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
            result = esHeap $ runST (calculate [] $ readProgram initialState)
        A.elems result `shouldBe` finalState

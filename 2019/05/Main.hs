{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE NumericUnderscores   #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE TupleSections        #-}
{-# LANGUAGE UndecidableInstances #-}

import           Control.Applicative.Combinators
import           Control.Monad
import           Control.Monad.ST
import           Data.Array                      (Ix)
import           Data.Array.MArray               (readArray, writeArray)
import qualified Data.Array.MArray               as MA
import           Data.Array.ST                   (STUArray, runSTUArray)
import           Data.Array.Unboxed              (UArray)
import qualified Data.Array.Unboxed              as A
import           Data.Attoparsec.Text            (char, decimal, signed)
import           Data.Functor.Identity
import           Data.List                       (minimumBy)
import           Data.Maybe
import           Data.Ord                        (comparing)
import           Data.Word
import           Elves
import           Elves.Advent
import Data.Function ((&))
import Control.Lens (view, makeLenses, (^.), (.~), (%~))
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
                 | Add Parameter Parameter Addr
                 | Mul Parameter Parameter Addr
                 | Input Addr
                 | Output Parameter
                 deriving (Show, Eq)

data ExecutionState i h = ExecutionState
  { _esKont   :: i
  , _esInput  :: Stream
  , _esOutput :: Stream
  , _esHeap   :: h
  } deriving (Show, Eq)

makeLenses ''ExecutionState

initialState :: ExecutionState Addr ()
initialState = ExecutionState 0 [] [] ()

type Offset = Addr -> Addr

offset :: Instruction -> Addr -> Addr
offset Exit = id
offset (Input _) = (+ 2)
offset (Output _) = (+ 2)
offset (Add _ _ _) = (+ 4)
offset (Mul _ _ _) = (+ 4)

main :: IO ()
main = day 5 parse pt1 pt2 test
  where
    parse = fmap readProgram (signed decimal `sepBy1` char ',')
    pt1 = const (print ())
    pt2 = const (print ())

value0 :: Program -> Value
value0 = (A.! 0)

calculate :: Stream -> Program -> ST s (Either String (ExecutionState Addr Program))
calculate input prog = do
  heap <- MA.thaw prog
  let exit i s = do h <- MA.freeze heap
                    let s' = s & esKont .~ i & esHeap .~ h
                    pure (pure s')
  let go addr ms  = case ms of
                      Left err -> pure (Left err)
                      Right s -> do let addr' = addr & s^.esKont
                                    mi <- nextInstruction addr' heap
                                    case mi of
                                      Nothing -> pure . Left $ "No instruction at " <> show addr'
                                      Just Exit -> exit addr' s 
                                      Just i -> applyInstruction (s & esKont .~ i) >>= go addr'

  go 0 . pure $ initialState & esInput .~ input
                             & esHeap .~ heap
                             & esKont .~ id

readProgram :: [Value] -> Program
readProgram vals = A.listArray (0, fromIntegral (length vals) - 1) vals

applyInstruction :: ExecutionState Instruction (Heap s) -> ST s (Either String (ExecutionState Offset (Heap s)))
applyInstruction state = advance <$> case instruction of
  Exit      -> pure $ pure state
  Add a b r -> op (+) a b r
  Mul a b r -> op (*) a b r
  Output p  -> do
    bs <- MA.getBounds (state^.esHeap)
    mx <- resolve bs p
    case mx of
      Nothing -> boom "Reference error"
      Just v  -> pure . pure $ state & esOutput %~ (v :)
  Input a   -> case state^.esInput of
    [] -> boom "Unexpected end of input"
    (x:xs) -> do MA.writeArray (state^.esHeap) a x
                 pure . pure $ state & esInput .~ xs
  where
    advance = fmap (esKont .~ offset instruction)
    instruction = state ^. esKont
    resolve _ (Immediate v)    = pure (pure v)
    resolve bs (Position addr) = tryRead (state^.esHeap) bs addr
    boom = pure . Left
    op f a b r = do
      bs <- MA.getBounds (state^.esHeap)
      mx <- resolve bs a
      my <- resolve bs b
      case liftA2 f mx my of
        Nothing -> boom "Reference error"
        Just res -> pure state <$ MA.writeArray (state^.esHeap) r res

nextInstruction :: Addr -> Heap s -> ST s (Maybe Instruction)
nextInstruction index heap = do
  bs <- MA.getBounds heap
  code <- get bs index
  case fmap (`quotRem` 100) code of
    Just (0, 99) -> pure (Just Exit)
    Just (ms,  1) -> instr2 (modes ms) bs Add
    Just (ms,  2) -> instr2 (modes ms) bs Mul
    Just (_,  3) -> fmap Input <$> (fmap fromIntegral <$> get bs (index + 1))
    Just (ms,  4) -> let m = head (modes ms)
                      in fmap Output <$> (fmap m <$> get bs (index + 1))
    _            -> pure Nothing
  where
    get = tryRead heap
    instr2 (ma:mb:_) bs f = liftM3 f <$> (fmap ma <$> get bs (index + 1))
                                     <*> (fmap mb <$> get bs (index + 2))
                                     <*> (fmap fromIntegral <$> get bs (index + 3))
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
    let newState h inp i = initialState & esHeap .~ h & esInput .~ inp & esKont .~ i
    let examples =
         [ ([1,0,0,0,99], Add (Position 0) (Position 0) 0, [2,0,0,0,99])
         , ([1,0,0,0,99], Exit, [1,0,0,0,99])
         , ([2,3,0,3,99], Mul (Position 3) (Position 0) 3, [2,3,0,6,99])
         , ([2,3,0,3,99], Input 1, [2,10000,0,3,99])
         , ([1,9,10,3,2,3,11,0,99,30,40,50], Mul (Position 3) (Position 11) 0, [150,9,10,3,2,3,11,0,99,30,40,50])
         ]
    forM_ examples $ \(state, command, expected) -> do
      it ("gets the right result for: applyInstruction " <> show command <> " " <> show state) $ do
        let prog = readProgram state
            result = runST $ do heap <- MA.thaw prog
                                applyInstruction (newState heap [10000 ..] command)
                                MA.freeze heap
        result `shouldBe` readProgram expected
    it "always leaves a program untouched when we exit" . property $ \ints ->
      let prog = readProgram ints
          result = runST $ do heap <- MA.thaw prog
                              applyInstruction (newState heap mempty Exit)
                              MA.freeze heap
       in result == prog
    it "handles positional output instructions" $ do
      let prog = readProgram (take 10 [1 ..])
          result = runST $ do
                     heap <- MA.thaw prog
                     s <- applyInstruction (newState heap mempty (Output (Position 5)))
                     pure (view esOutput <$> s)
      result `shouldBe` Right [6]
    it "handles immediate output instructions" $ do
      let prog = readProgram (take 10 [1 ..])
          result = runST $ do
                     heap <- MA.thaw prog
                     s <- applyInstruction (newState heap mempty (Output (Immediate 5)))
                     pure (view esOutput <$> s)
      result `shouldBe` Right [5]
    it "does not mutate the heap on output" . property $ \ints ->
      let prog = readProgram ints
          result = runST $ do heap <- MA.thaw prog
                              let s = newState heap mempty (Output (Immediate 5))
                              s' <- applyInstruction s
                              _  <- applyInstruction s { _esKont = Output (Position 5) }
                              MA.freeze heap
       in result == prog

  describe "nextInstruction" $ do
    let examples = [ (0, [1,0,0,0,99], Just (Add (Position 0) (Position 0) 0))
                   , (4, [1,0,0,0,99], Just Exit)
                   , (0, [2,3,0,3,99], Just (Mul (Position 3) (Position 0) 3))
                   , (0, [1002,3,0,3,99], Just (Mul (Position 3) (Immediate 0) 3))
                   , (0, [102,3,0,3,99], Just (Mul (Immediate 3) (Position 0) 3))
                   , (0, [101,3,0,3,99], Just (Add (Immediate 3) (Position 0) 3))
                   , (0, [1001,3,0,3,99], Just (Add (Position 3) (Immediate 0) 3))
                   , (4, [1,9,10,3,2,3,11,0,99,30,40,50], Just (Mul (Position 3) (Position 11) 0))
                   , (8, [1,9,10,3,2,3,11,0,99,30,40,50], Just Exit)
                   , (9, [1,9,10,3,2,3,11,0,99,30,40,50], Nothing)
                   , (0, [3,3,0,3,99], Just (Input 3))
                   , (0, [4,3,0,3,99], Just (Output (Position 3)))
                   , (0, [104,3,0,3,99], Just (Output (Immediate 3)))
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
            result = A.elems . view esHeap <$> runST (calculate [] $ readProgram initialState)
        result `shouldBe` Right finalState

{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}

import           Control.Applicative
import           Control.Lens
import           Control.Lens.TH
import qualified Data.Array              as A
import           Data.Array.Base         (unsafeAt)
import           Data.Attoparsec.Text    (Parser, parseOnly)
import           Data.Bits
import           Data.Bool
import           Data.Char
import           Data.Foldable           (foldl')
import           Data.Hashable
import qualified Data.Map.Strict         as M
import           Data.Text               (Text)
import qualified Data.Text               as Text
import qualified Data.Text.IO            as Text
import           GHC.Generics
import           System.Environment
import           System.Exit
import           Text.Parser.Char
import           Text.Parser.Combinators (choice, sepBy1)
import           Text.Printf

data Memory = Memory {
  _regA,_regB,_regC,_regD,_regE,_regF :: !Int
  } deriving (Show, Eq, Generic)

instance Hashable Memory

makeLenses ''Memory

newtype MemoryLens = MemoryLens { ml :: Lens' Memory Int }

data Argument = Register | Value deriving (Show)

data Instruction = Addr
                 | Addi
                 | Mulr
                 | Muli
                 | Banr
                 | Bani
                 | Borr
                 | Bori
                 | Setr
                 | Seti
                 | GtIR
                 | GtRI
                 | GtRR
                 | EqIR
                 | EqRI
                 | EqRR
                 deriving (Show, Eq, Ord, Enum, Bounded)

data ByteCode a = ByteCode
  { opCode :: a
  , args   :: (Int, Int, Int)
  } deriving (Show, Eq, Functor)

data Input = Input
  { instructionPtr :: MemoryLens
  , programme      :: A.Array Int (ByteCode Instruction)
  }

inputP :: Parser Input
inputP = Input <$> (instructionPtrP <* newline)
               <*> fmap mkProg (instrP `sepBy1` newline)
  where
    mkProg is = A.listArray (0,length is - 1) is

instructionPtrP :: Parser MemoryLens
instructionPtrP = do
  string "#ip "
  p <- intP
  maybe empty pure (reg p)

instrP :: Parser (ByteCode Instruction)
instrP = do
  op <- opP
  space
  args <- (,,) <$> (intP <* space) <*> (intP <* space) <*> intP
  return (ByteCode op args)

opP :: Parser Instruction
opP = choice [instr <$ string (fmap toLower $ show instr)
                 | instr <- [minBound .. maxBound]]

intP :: Parser Int
intP = read <$> some digit

-- either would be a bit more useful here, but Maybe is faster.
reg :: Int -> Maybe MemoryLens
reg 0 = pure (MemoryLens regA)
reg 1 = pure (MemoryLens regB)
reg 2 = pure (MemoryLens regC)
reg 3 = pure (MemoryLens regD)
reg 4 = pure (MemoryLens regE)
reg 5 = pure (MemoryLens regF)
reg n = Nothing

{-# INLINE rrOp #-}
{-# INLINE riOp #-}
rrOp, riOp :: (Int -> Int -> Int) -> Memory -> (Int, Int, Int) -> Maybe Memory

rrOp f m (a,b,c) = do
  la <- reg a
  lb <- reg b
  lc <- reg c
  return $ m & (ml lc) .~ (f (m ^. (ml la)) (m ^. (ml lb)))

riOp f m (a,b,c) = do
  la <- reg a
  lc <- reg c
  return $ m & (ml lc) .~ (f (m ^. (ml la)) b)

{-# INLINE cmpIR #-}
{-# INLINE cmpRI #-}
{-# INLINE cmpRR #-}
cmpIR, cmpRI, cmpRR :: Ordering -> Memory -> (Int, Int, Int) -> Maybe Memory

cmpIR cmp m (a,b,c) = do
  lb <- reg b
  lc <- reg c
  let ret = bool 0 1 (compare a (m ^. ml lb) == cmp)
  return $ m & (ml lc) .~ ret

cmpRI cmp m (a,b,c) = do
  la <- reg a
  lc <- reg c
  let ret = bool 0 1 (compare (m ^. ml la) b == cmp)
  return $ m & (ml lc) .~ ret

cmpRR cmp m (a,b,c) = do
  la <- reg a
  lb <- reg b
  lc <- reg c
  let ret = bool 0 1 (compare (m ^. ml la) (m ^. ml lb) == cmp)
  return $ m & (ml lc) .~ ret

{-# INLINE eval #-}
eval :: Instruction -> Memory -> (Int, Int, Int) -> Maybe Memory
eval Addr = rrOp (+)
eval Addi = riOp (+)
eval Mulr = rrOp (*)
eval Muli = riOp (*)
eval Banr = rrOp (.&.)
eval Bani = riOp (.&.)
eval Borr = rrOp (.|.)
eval Bori = riOp (.|.)
eval Setr = \m (a,_,c) -> do
  la <- reg a
  lc <- reg c
  return $ m & (ml lc) .~ (m ^. ml la)
eval Seti = \m (a,_,c) -> do
  lc <- reg c
  return $ m & (ml lc) .~ a
eval GtIR = cmpIR GT
eval GtRI = cmpRI GT
eval GtRR = cmpRR GT
eval EqIR = cmpIR EQ
eval EqRI = cmpRI EQ
eval EqRR = cmpRR EQ

exampleInput :: Text
exampleInput = Text.unlines
  ["#ip 0"
  ,"seti 5 0 1"
  ,"seti 6 0 2"
  ,"addi 0 1 0"
  ,"addr 1 2 3"
  ,"setr 1 0 0"
  ,"seti 8 0 4"
  ,"seti 9 0 5"
  ]

main :: IO ()
main = do
  einp <- parseOnly inputP <$> Text.getContents
  case einp of
    Left err -> die err
    Right prg -> do
      args <- getArgs
      case args of
        ["pt1"] -> print $ runProgramme prg newMemory
        ["pt2"] -> print $ runProgramme prg (newMemory & regA .~ 1)
        _       -> die $ "Bad arguments : " ++ show args

newMemory :: Memory
newMemory = Memory 0 0 0 0 0 0

runProgramme :: Input -> Memory -> Memory
runProgramme input = go 0
  where
    ipReg    = instructionPtr input
    instrs   = programme input

    go ip m = case instrs ^? ix ip of
                Nothing           -> m
                Just ByteCode{..} -> case eval opCode (m & ml ipReg .~ ip) args of
                                       Nothing  -> error "register error"
                                       Just mem -> mem `seq` go (1 + mem ^. ml ipReg) mem


{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TupleSections     #-}

import           Control.Applicative
import           Control.Lens
import           Control.Monad.ST
import qualified Data.Array              as A
import           Data.Array.Base         (unsafeRead, unsafeWrite)
import qualified Data.Array.ST           as SA
import qualified Data.Array.Unboxed      as UA
import           Data.Attoparsec.Text    (Parser, parseOnly)
import           Data.Bits
import           Data.Bool
import           Data.Text               (Text)
import qualified Data.Text               as Text
import qualified Data.Text.IO            as Text
import           System.Environment
import           System.Exit
import           Text.Parser.Char
import           Text.Parser.Combinators (choice, sepBy1)

type Memory = UA.UArray Int Int

newtype Register = Register Int deriving (Show, Eq)

newtype ActiveMemory s = ActiveMemory { activeMemory :: SA.STUArray s Int Int }

data OpCode = Add | Mul | Band | Bor | Set | Gtr | Equ deriving (Show, Eq)

readRegister :: Register -> ActiveMemory s -> ST s Int
readRegister (Register i) (ActiveMemory m) = unsafeRead m i

writeRegister :: ActiveMemory s -> Register -> Int -> ST s ()
writeRegister (ActiveMemory m) (Register i) v = unsafeWrite m i v

-- we avoid illegal register access by guaranteeing that register
-- access is legal at parse-time.
data ByteCode
             = RROperator
               { opCode :: OpCode
               , regA   :: !Register
               , regB   :: !Register
               , retReg :: !Register
               }
             | RIOperator
               { opCode :: OpCode
               , regA   :: !Register
               , valB   :: !Int
               , retReg :: !Register
               }
             | IROperator
               { opCode :: OpCode
               , valA   :: !Int
               , regB   :: !Register
               , retReg :: !Register
               }
             | IAnyOperator
               { opCode :: OpCode
               , valA   :: !Int
               , retReg :: !Register
               }
              deriving (Show, Eq)

data Input = Input
  { instructionPtr :: Register
  , programme      :: A.Array Int ByteCode
  }

inputP :: Parser Input
inputP = Input <$> (instructionPtrP <* newline)
               <*> fmap mkProg (instrP `sepBy1` newline)
  where
    mkProg is = A.listArray (0,length is - 1) is

instructionPtrP :: Parser Register
instructionPtrP = string "#ip " *> regP

instrP :: Parser ByteCode
instrP = choice [rrOpP, riOpP, irOpP, iAnyP]

rrOpP = RROperator <$> choice [Add  <$ string "addr"
                              ,Mul  <$ string "mulr"
                              ,Band <$ string "banr"
                              ,Bor  <$ string "borr"
                              ,Gtr  <$ string "gtrr"
                              ,Equ  <$ string "eqrr"
                              ]
                  <*> (space *> regP)
                  <*> (space *> regP)
                  <*> (space *> regP)

riOpP = RIOperator <$> choice [Add  <$ string "addi"
                              ,Mul  <$ string "muli"
                              ,Band <$ string "bani"
                              ,Bor  <$ string "bori"
                              ,Gtr  <$ string "gtri"
                              ,Equ  <$ string "eqri"
                              ,Set  <$ string "setr"
                              ]
                  <*> (space *> regP)
                  <*> (space *> intP)
                  <*> (space *> regP)

-- only valid for comparisons
irOpP = IROperator <$> choice [Gtr  <$ string "gtir"
                              ,Equ  <$ string "eqir"
                              ]
                  <*> (space *> intP)
                  <*> (space *> regP)
                  <*> (space *> regP)

-- only valid for seti
iAnyP = IAnyOperator <$> (Set  <$ string "seti")
                  <*> (space *> intP)
                  <*> (space >> intP >> space *> regP)

regP :: Parser Register
regP = intP >>= maybe empty pure . reg

intP :: Parser Int
intP = read <$> some digit

-- either would be a bit more useful here, but Maybe is faster.
reg :: Int -> Maybe Register
reg n = if A.inRange (0,5) n then pure $ Register n
                             else Nothing

{-# INLINE eval #-}
eval :: ByteCode -> ActiveMemory s -> ST s ()
eval bc m = op bc >>= writeRegister m (retReg bc)
  where
        f Add  = (+)
        f Mul  = (*)
        f Band = (.&.)
        f Bor  = (.|.)
        f Set  = const
        f Gtr  = cmp GT
        f Equ  = cmp EQ

        cmp ord a b = bool 0 1 (compare a b == ord)

        op RROperator{..}   = f opCode <$> readRegister regA m <*> readRegister regB m
        op RIOperator{..}   = f opCode <$> readRegister regA m <*> pure valB
        op IROperator{..}   = f opCode valA <$> readRegister regB m
        op IAnyOperator{..} = pure (f opCode valA 0)

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
  args <- getArgs
  case args of
    ["pt1"]  -> run newMemory
    ["pt2"]  -> run (newMemory & ix 0 .~ 1)
    ["test"] -> do let (Right prg) = parseOnly inputP exampleInput
                       ret = UA.elems (runProgramme prg newMemory)
                   if ret == [6, 5, 6, 0, 0, 9]
                      then putStrLn "OK"
                      else die $ "FAIL: " ++ show ret
    _       -> die $ "Bad arguments : " ++ show args
  where
    run mem = do
      einp <- parseOnly inputP <$> Text.getContents
      case einp of
        Left err  -> die err
        Right prg -> print $ runProgramme prg mem

newMemory :: Memory
newMemory = UA.listArray (0,5) (repeat 0)

runProgramme :: Input -> Memory -> Memory
runProgramme input mem = SA.runSTUArray (SA.thaw mem >>= run . ActiveMemory)
  where
    ipReg    = instructionPtr input
    instrs   = programme input

    run m = let go ip = case instrs ^? ix ip of
                          Nothing -> return (activeMemory m)
                          Just op -> do writeRegister m ipReg ip
                                        eval op m
                                        ip' <- readRegister ipReg m
                                        go (1 + ip')
             in go 0

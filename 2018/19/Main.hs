{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
import           Control.Applicative
import           Control.Lens
import           Control.Monad
import           Control.Monad.ST
import qualified Data.Array              as A
import           Data.Array.Base         (unsafeRead, unsafeWrite)
import qualified Data.Array.ST           as SA
import qualified Data.Array.Unboxed      as UA
import qualified Data.Set as S
import qualified Data.Map.Strict as M
import           Data.Attoparsec.Text    (Parser, parseOnly)
import           Data.Bits
import           Data.Bool
import           Data.Text               (Text)
import qualified Data.Text               as Text
import qualified Data.Text.IO            as Text
import           System.Environment
import           System.Exit
import           Text.Parser.Char
import           Text.Parser.Combinators (eof, choice, sepBy1)
import Text.Printf

import Elves
import Elves.Advent

type BreakPoint = Int
type Memory = UA.UArray Int Int

newtype Register = Register Int deriving (Show, Eq)

newtype ActiveMemory s = ActiveMemory { activeMemory :: SA.STUArray s Int Int }

data OpCode = Add | Mul | Band | Bor | Set | Gtr | Equ deriving (Show, Eq)

readRegister :: Register -> ActiveMemory s -> ST s Int
readRegister (Register i) (ActiveMemory m) = unsafeRead m i

writeRegister :: ActiveMemory s -> Register -> Int -> ST s ()
writeRegister (ActiveMemory m) (Register i) = unsafeWrite m i

data DebugCommand = SetReg Register Int
                  | Quit
                  | DumpMemory
                  | NoCommand
                  | Continue
                  deriving (Show, Eq)

commandP :: Parser DebugCommand
commandP = choice [Quit <$ string ":q"
                  ,DumpMemory <$ string ":d"
                  ,Continue <$ string ":c"
                  ,SetReg <$> namedReg <*> (string " = " *> intP)
                  ,NoCommand <$ eof
                  ]
  where
    namedReg = choice [Register i <$ char c | (i,c) <- zip [0 ..] "abcde"]

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

-- an efficient way to actually solve this problem. 
-- See input.symbolic for the reasoning behind this.
sumOfFactors :: Int -> Int
sumOfFactors f =
  sum [ b + if e == b then 0 else e | b <- takeWhile ((<= f) . join (*)) [1 ..]
      , let (e,r) = f `divMod` b
      , r == 0
      ]

-- 17: f += 2     # <- programme starts here
-- 18: f *= f
-- 19: f *= 19
-- 20: f *= 11
-- 21: c += 5
-- 22: c *= 22
-- 23: c += 21
-- 24: f += c
-- 25: skip a     # <-- pt1. when a = 0, we start with f = 967
-- 26: goto 1     # Begin the loop
-- 27: c = 27
-- 28: c *= 28
-- 29: c += 29
-- 30: c *= 30
-- 31: c *= 14
-- 32: c *= 32
-- 33: f = f + c
runNative :: IO ()
runNative = do
  let f = product [4, 19, 11]
      c = 5 * 22 + 21
      c' = ((27 * 28) + 29) * product [30, 14, 32]
      target = f + c + c'
  putStrLn $ "Target: " <> show target
  putStrLn $ "Result: " <> show (sumOfFactors target)

main :: IO ()
main = generalDay 19 inputP spec
       [("pt1", print . runProgramme newMemory)
       ,("pt2", const runNative)
       ,("symbolise", putStrLn . symbolicly)
       ]

spec = describe "runProgramme" $ do
  let Right prg = parseOnly inputP exampleInput

  it "can run the example input" $ do
    let ret = UA.elems (runProgramme newMemory prg)

    ret `shouldBe` [6, 5, 6, 0, 0, 9]

newMemory :: Memory
newMemory = UA.listArray (0,5) (repeat 0)

runProgramme :: Memory -> Input -> Memory
runProgramme mem Input{..} = SA.runSTUArray (SA.thaw mem >>= run . ActiveMemory)
  where
    run m = let go ip = case programme ^? ix ip of
                          Nothing -> return (activeMemory m)
                          Just op -> do writeRegister m instructionPtr ip
                                        eval op m
                                        ip' <- readRegister instructionPtr m
                                        go (1 + ip')
             in go (fst $ A.bounds programme) -- start at first instruction

dumpMemory :: Memory -> IO ()
dumpMemory = mapM_ putStrLn . fmap (uncurry showBinding) . UA.assocs

showBinding reg val = unwords [pure (toEnum $ fromEnum 'a' + reg), "=", show val]

data Observations = Observations
  { breakpoints :: S.Set BreakPoint
  , observed    :: M.Map BreakPoint Register
  , termination :: M.Map BreakPoint (Register, (Int -> Bool))
  }

breakPoint b = Observations (S.singleton b) mempty mempty

breaks i = S.member i . breakpoints
observe i = M.lookup i . observed

terminate :: Int -> Memory -> Observations -> Bool
terminate i m os = case M.lookup i (termination os) of
                     Nothing -> False
                     Just (Register i, f) -> f (m UA.! i)

runProgrammeWithBreakPoints :: Observations -> Memory -> Input -> IO Memory
runProgrammeWithBreakPoints o mem inp = go mem 0
  where
    (Register ip) = instructionPtr inp

    go m instr | breaks instr o = do
      putStrLn $ "at: " ++ show instr
      dumpMemory m
      breakPoint m instr

    go m instr | terminate instr m o = pure m

    go m instr | Just (Register i) <- observe instr o = do
      putStrLn $ "at: " ++ show instr
      putStrLn $ showBinding i (m UA.! i)
      run m instr

    go m instr = run m instr

    breakPoint m instr = do
      ec <- parseOnly commandP <$> Text.getLine
      case ec of
        Left err -> putStrLn err >> breakPoint m instr
        Right c -> case c of
                     Quit -> return m
                     NoCommand -> breakPoint m instr
                     Continue -> run m instr
                     DumpMemory -> dumpMemory m >> breakPoint m instr
                     SetReg (Register i) v -> breakPoint (m UA.// [(i,v)]) instr

    run m instr | not (A.inRange (A.bounds (programme inp)) instr) = pure m
    run m instr = do
      let subp = subProgramme (instr,instr) inp
          m'   = runProgramme m subp 
      go m' (1 + m' UA.! ip)

--- helper to make instructions more intelligible for reverse engineering
symbolicly :: Input -> String
symbolicly Input{..} = unlines $ fmap instr (A.assocs programme)
  where
    symbol addr r | r == instructionPtr = show addr
    symbol _    (Register i)            = pure $ toEnum (fromEnum 'a' + i)
    instr (addr, bc) = printf "%02d: " addr ++ symbolise addr bc
    symbolise _    IAnyOperator{opCode = Set, valA = i, retReg = r} | r == instructionPtr = "goto " ++ show (i + 1)
    symbolise addr bc | retReg bc == instructionPtr = "goto 1 + " ++ bcSym addr bc
    symbolise addr bc = symbol addr (retReg bc) ++ " = " ++ bcSym addr bc

    bcSym a RROperator{..} = unwords [symbol a regA, op opCode, symbol a regB]
    bcSym a RIOperator{..} = unwords [symbol a regA, op opCode, show valB]
    bcSym a IROperator{..} = unwords [show valA, op opCode, symbol a regB]
    bcSym _ IAnyOperator{..} = unwords [op opCode, show valA]

    op Add  = "+"
    op Mul  = "*"
    op Band = "&"
    op Bor  = "|"
    op Set  = ""
    op Gtr  = ">"
    op Equ  = "=="
    
subProgramme :: (Int, Int) -> Input -> Input
subProgramme bs inp = inp { programme = A.array bs (filter ((A.inRange bs) . fst) (A.assocs (programme inp))) }

                    

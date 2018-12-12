{-# LANGUAGE DeriveFunctor #-}

import           Control.Applicative
import           Data.Array.Unboxed           ((//))
import qualified Data.Array.Unboxed           as A
import qualified Data.HashMap.Strict          as HM
import           Data.List                    (foldl', nub, sort)
import           Data.Maybe
import           Data.Text                    (Text)
import qualified Data.Text                    as Text
import           Text.Parser.Char
import           Text.Parser.Combinators      (choice, sepBy1)
import           Text.ParserCombinators.ReadP (ReadP, readP_to_S)
import           Text.Read                    (readMaybe)

type Register = Text
type Addr = Int
type Memory = A.UArray Addr Int
data Action = Inc Int | Dec Int deriving (Show)
data Comparison = Gt | Gte | Lt | Lte | Equ | Neq deriving (Show)

data Condition a = Condition
  { condRegister   :: a
  , condComparison :: Comparison
  , condComparand  :: Int
  } deriving (Show, Functor)

data Instruction a = Instruction
  { register  :: a
  , action    :: Action
  , condition :: Condition a
  } deriving (Show, Functor)

type Program a = [Instruction a]

main :: IO ()
main = do
  inp <- getContents
  let (maxval, mem) = runCode inp
  putStrLn $ "Highest value seen: " ++ show maxval
  putStrLn $ "Current highest value: " ++ show (maximum $ A.elems mem)

runCode :: String -> (Int, Memory)
runCode code = do
  case runParser programP code of
    Nothing -> (0, A.array (0,0) [])
    Just p -> let (p', mem) = initialise p
               in run (0, mem) p'

initialise :: Program Register -> (Program Addr, Memory)
initialise p =
  let regs = nub $ sort (p >>= allRegisters)
      mem = A.array (0, length regs - 1) (zip [0 ..] (pure 0 <$> regs))
      translation = HM.fromList (zip regs [0 ..])
   in (fmap (fmap (translation HM.!)) p, mem)

allRegisters :: Instruction a -> [a]
allRegisters instr = [register instr, condRegister (condition instr)]

run :: (Int, Memory) -> Program Addr -> (Int, Memory)
run = foldl' $ \(highest, mem) instr ->
  if evalCond mem (condition instr)
   then let mem' = mem // [evalAction mem (register instr) (action instr)]
         in (max highest (maximum $ A.elems mem'), mem')
   else (highest, mem)

evalAction :: Memory -> Addr -> Action -> (Addr, Int)
evalAction mem reg act = let val = mem A.! reg
                             f = case act of
                                   Inc x -> (+ x)
                                   Dec x -> (subtract x)
                          in (reg, f val)

evalCond :: Memory -> Condition Addr -> Bool
evalCond mem c = let val = mem A.! (condRegister c)
                     f   = case condComparison c of
                             Gt  -> (>)
                             Gte -> (>=)
                             Lt  -> (<)
                             Lte -> (<=)
                             Equ -> (==)
                             Neq -> (/=)
                  in f val (condComparand c)

runParser :: ReadP a -> String -> Maybe a
runParser p = fmap fst . listToMaybe . reverse . readP_to_S p

programP :: ReadP (Program Register)
programP = instrP `sepBy1` newline

instrP :: ReadP (Instruction Register)
instrP = Instruction <$> registerP
                     <*> (space         *> actionP)
                     <*> (string " if " *> conditionP)

registerP :: ReadP Register
registerP = Text.pack <$> some letter

actionP :: ReadP Action
actionP = choice [Inc <$ string "inc ", Dec <$ string "dec "] <*> intP

intP :: ReadP Int
intP = (fromMaybe id <$> optional (negate <$ char '-')) <*> (read <$> some digit)

conditionP :: ReadP (Condition Register)
conditionP = Condition <$> registerP <*> (space *> comparisonP) <*> (space *> intP)

comparisonP :: ReadP Comparison
comparisonP = choice [Gte <$ string ">="
                     ,Lte <$ string "<="
                     ,Equ <$ string "=="
                     ,Neq <$ string "!="
                     ,Gt  <$ string ">"
                     ,Lt  <$ string "<"
                     ]

exampleInput :: String
exampleInput = unlines
  [ "b inc 5 if a > 1"
  , "a inc 1 if b < 5"
  , "c dec -10 if a >= 1"
  , "c inc -20 if c == 10"
  ]

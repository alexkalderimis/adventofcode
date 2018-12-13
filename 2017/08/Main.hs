{-# LANGUAGE DeriveFunctor #-}

import qualified Data.Time.Clock as Clock
import           Control.Applicative
import           Control.Monad
import           Control.Monad.ST
import qualified Data.Array                   as A
import           Data.Array.ST
import qualified Data.HashMap.Strict          as HM
import           Data.List                    (nub, sort)
import           Data.Maybe
import           Data.Text                    (Text)
import qualified Data.Text                    as Text
import           System.Exit
import           Text.Parser.Char
import           Text.Parser.Combinators      (choice, sepBy1)
import           Text.ParserCombinators.ReadP (ReadP, readP_to_S)
import           Text.Read                    (readMaybe)
import Data.Int

type Register = Text
type Addr = Int8
type Memory s = STUArray s Addr Int16
data Action = Inc Int16 | Dec Int16 deriving (Show)
data Comparison = Gt | Gte | Lt | Lte | Equ | Neq deriving (Show)

data Condition a = Condition
  { condRegister   :: a
  , condComparison :: Comparison
  , condComparand  :: Int16
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
  case runParser programP inp of
    Nothing -> die "Cannot parse instructions"
    Just p -> time $ do
      let (maxval, mem) = runST $ do (p', mem) <- initialise p
                                     mv <- run mem p'
                                     imem <- freeze mem
                                     return (mv, imem)

      putStrLn $ "No. of registers: " ++ show (A.rangeSize $ A.bounds mem)
      putStrLn $ "Highest value seen: " ++ show maxval
      putStrLn $ "Current highest value: " ++ show (maximum $ A.elems mem)
  where
    time act = do
      start <- Clock.getCurrentTime
      act
      end <- Clock.getCurrentTime
      print (Clock.diffUTCTime end start)

initialise :: Program Register -> ST s (Program Addr, Memory s)
initialise p = do
  let regs = nub $ sort (p >>= allRegisters)
      translation = HM.fromList (zip regs [0 ..])
      prog = fmap (fmap (translation HM.!)) p
  mem <- newArray (0, toEnum (length regs - 1)) 0
  return (prog, mem)

allRegisters :: Instruction a -> [a]
allRegisters instr = [register instr, condRegister (condition instr)]

run :: Memory s -> Program Addr -> ST s Int16
run mem = (fmap maximum .) . mapM $ \instr -> do
  c <- evalCond mem (condition instr)
  when c $ do
    val <- evalAction mem (register instr) (action instr)
    writeArray mem (register instr) val
  maximum <$> getElems mem

evalAction :: Memory s -> Addr -> Action -> ST s Int16
evalAction mem reg act = do
  val <- readArray mem reg
  let f = case act of Inc x -> (+ x)
                      Dec x -> (subtract x)
  return (f val)

evalCond :: Memory s -> Condition Addr -> ST s Bool
evalCond mem c = do
  val <- readArray mem (condRegister c)
  let f = case condComparison c of
             Gt  -> (>)
             Gte -> (>=)
             Lt  -> (<)
             Lte -> (<=)
             Equ -> (==)
             Neq -> (/=)
  return (f val (condComparand c))

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

intP :: ReadP Int16
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

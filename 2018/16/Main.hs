{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}

import           Control.Applicative
import           Control.Lens
import qualified Data.Array              as A
import           Data.Attoparsec.Text    (Parser, parseOnly)
import           Data.Bits
import           Data.Bool
import           Data.Foldable           (foldl')
import qualified Data.Map.Strict         as M
import qualified Data.Set                as S
import           Data.Text               (Text)
import qualified Data.Text               as Text
import qualified Data.Text.IO            as Text
import           System.Environment
import           System.Exit
import           Text.Parser.Char
import           Text.Parser.Combinators (between, sepBy1)

import Elves
import Elves.Advent

data ByteCodeInstr a = ByteCodeInstr
  { opCode :: a
  , args   :: (Int, Int, Int)
  } deriving (Show, Eq, Functor)

data Sample = Sample
  { memBefore     :: Memory
  , memAfter      :: Memory
  , byteCodeInstr :: ByteCodeInstr Int
  } deriving (Show, Eq)

data Input = Input
  { samples   :: [Sample]
  , programme :: [ByteCodeInstr Int]
  } deriving (Show, Eq)

main :: IO ()
main = day 16 inputP pt1 pt2 (it "has tests" pending)
  where
    pt1 = print . length . filter ((>= 3) . length . couldBe) . samples
    pt2 (Input samples prog) =  do
      let mapping = solve samples
      let prog' = fmap (mapping M.!) <$> prog
      putStrLn . maybe "NUL" (show . view _1) $ runProgramme prog'

inputP :: Parser Input
inputP = Input <$> (sampleP `sepBy1` (newline >> newline))
               <*> (some newline >> (instrP `sepBy1` newline))

sampleP :: Parser Sample
sampleP = do
  m0 <- string "Before:" >> some space >> memoryP
  newline
  bc <- instrP
  newline
  m1 <- string "After:" >> some space >> memoryP
  return (Sample m0 m1 bc)

memoryP :: Parser Memory
memoryP = between (char '[') (char ']') $
  (,,,) <$> (intP <* string ", ")
        <*> (intP <* string ", ")
        <*> (intP <* string ", ")
        <*> intP

instrP :: Parser (ByteCodeInstr Int)
instrP = do
  op <- intP
  space
  args <- (,,) <$> (intP <* space) <*> (intP <* space) <*> intP
  return (ByteCodeInstr op args)

intP :: Parser Int
intP = read <$> some digit

type Memory = (Int, Int, Int, Int)

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

reg :: Int -> Maybe MemoryLens
reg 0 = Just (MemoryLens _1)
reg 1 = Just (MemoryLens _2)
reg 2 = Just (MemoryLens _3)
reg 3 = Just (MemoryLens _4)
reg _ = Nothing

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

exampleSample :: Text
exampleSample = Text.unlines
  ["Before: [3, 2, 1, 1]"
  ,"9 2 1 2"
  ,"After:  [3, 2, 2, 1]"
  ]

couldBe :: Sample -> [Instruction]
couldBe (Sample m0 m1 bc) = filter f [minBound .. maxBound]
  where
    f instr = eval instr m0 (args bc) == Just m1

cantBe :: Sample -> [Instruction]
cantBe (Sample m0 m1 bc) = filter f [minBound .. maxBound]
  where
    f instr = eval instr m0 (args bc) /= Just m1

solve :: [Sample] -> M.Map Int Instruction
solve ss =
  let cannot = M.fromListWith (<>) [(opCode (byteCodeInstr s), S.fromList (cantBe s)) | s <- ss]
      possibles = M.map excluded cannot
   in go mempty possibles
  where
    excluded set = S.fromList $ filter (`S.notMember` set) [minBound .. maxBound]
    -- iteratively solve the puzzle by removing instructions we have sufficient
    -- evidence for.
    go :: M.Map Int Instruction -> M.Map Int (S.Set Instruction) -> M.Map Int Instruction
    go good maybes | M.null maybes = good
    go good maybes =
        let (found, remaining) = M.partition ((== 1) . S.size) maybes
         in if M.null found
              then error $ "Cannot solve " <> show maybes
              else let good' = good <> M.map (head . S.toList) found
                       maybes' = foldl' removeFound remaining $ M.elems good'
                    in go good' maybes'
    removeFound m k = M.map (S.delete k) m

runProgramme :: [ByteCodeInstr Instruction] -> Maybe Memory
runProgramme = foldl' go (Just (0,0,0,0))
  where
    go mm bc = mm >>= \m -> eval (opCode bc) m (args bc)


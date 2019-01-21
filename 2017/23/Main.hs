{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}

import           Control.Monad.State.Strict
import           Control.Monad.Writer.Strict
import           Data.Coerce                 (coerce)
import qualified Data.Foldable               as F
import           Data.Functor.Identity
import qualified Data.IntMap.Strict          as M
import           Data.Monoid
import           Data.Sequence               (Seq (..), (<|), (|>))
import qualified Data.Sequence               as Seq
import qualified Data.Text                   as Text
import           Data.Vector                 (Vector)
import qualified Data.Vector                 as V

import           Data.Attoparsec.Text        (decimal, letter, signed)
import           Text.Parser.Char            (newline)
import           Text.Parser.Combinators     (choice, sepBy1)

import           Elves
import           Elves.Advent

type Register = Char
type Value = Either Register Int
type Memory = M.IntMap Int
type Programme = Vector Instr

data ProgrammeState = ProgrammeState
  { instrPtr     :: Int
  , registers    :: Memory
  , instructions :: Programme
  } deriving (Show)

newtype DebugM a = DebugM { runDebugM :: WriterT (Endo [Instr])
                                                 (StateT ProgrammeState Identity)
                                                 a
                          }
  deriving (Functor, Applicative, Monad, MonadWriter (Endo [Instr]), MonadState ProgrammeState)

data Instr
 = Set Register Value
 | Add Register Value
 | Sub Register Value
 | Mul Register Value
 -- | Mod Register Value
 -- | Send Value
 -- | Receive Register
 | Jump Value Value
 deriving (Show, Eq)

main :: IO ()
main = day 18 (V.fromList <$> parser) pt1 pt2 test
  where
    parser = (instrP `sepBy1` newline)
    pt1 prg = let is = snd $ runUntilCompletion prg in print $ sum [1 | Mul{} <- is]
    -- for the logic here, see input.disassembled
    -- The programme we are given finds the number of non-primes
    -- in this particular range.
    pt2 _ = let xs = [106500, 106500 + 17 .. 123500]
            in print . length $ filter (not . isPrime) xs

runUntilCompletion :: Programme -> (Memory, [Instr])
runUntilCompletion instrs =
  let s = ProgrammeState 0 mempty instrs
      (mem,w) = fst . runIdentity $ runStateT (runWriterT (runDebugM go)) s
   in (mem, appEndo w [])
  where
    go = step >>= maybe go pure

-- very very naive trial division search, but more
-- than fast enough for our needs.
isPrime :: Int -> Bool
isPrime i = go 2 
  where
    lim = ceiling . sqrt $ realToFrac i
    go fac | fac > lim = True
    go fac = case i `mod` fac of
               0 -> False
               _ -> go (fac + 1)

test = do
   let parse = parseOnly (instrP `sepBy1` newline)
   describe "runUntilCompletion" $ do
     let mprg = parse examplePrg
     it "produces the correct result" $ do
       let (Right prg) = V.fromList <$> mprg
           (mem, is) = runUntilCompletion prg
       sum [1 | Mul{} <- is] `shouldBe` 100
   describe "example" $ do
     let mduet = parse examplePrg
     describe "instrP" $ do
       it "parses the example correctly" $ do
         mduet `shouldBe` Right [Set 'a' (Right 100)
                                ,Add 'b' (Right 2)
                                ,Mul 'b' (Left 'a')
                                ,Sub 'a' (Right 1)
                                ,Jump (Left 'a') (Right (-2))
                                ]

instrP = choice ["set " *> (Set <$> reg <*> (" " *> val))
                ,"add " *> (Add <$> reg <*> (" " *> val))
                ,"sub " *> (Sub <$> reg <*> (" " *> val))
                ,"mul " *> (Mul <$> reg <*> (" " *> val))
                -- ,"mod " *> (Mod <$> reg <*> (" " *> val))
                ,"jnz " *> (Jump <$> val <*> (" " *> val))
                -- , "snd " *> (Send <$> val)
                -- ,"rcv " *> (Receive <$> reg)
                ]
  where
    reg = letter
    val = choice [ Left <$> reg
                 , Right <$> signed decimal
                 ]

readRegister :: Register -> Memory -> Int
readRegister c = maybe 0 id . M.lookup (fromEnum c)

modifyRegister :: Register -> (Int -> Int) -> Memory -> Memory
modifyRegister c f = M.alter (pure . f . maybe 0 id) (fromEnum c)

value :: Memory -> Value -> Int
value m = either (`readRegister` m) id

-- run the duet using cooperative multi-tasking. Single threaded execution, with blocking
-- receive.
step :: DebugM (Maybe Memory)
step = do
  s <- get
  case instructions s V.!? instrPtr s of
    Nothing -> Just <$> gets registers
    Just i  -> tell (Endo (i:)) >> eval i >> pure Nothing

eval :: Instr -> DebugM ()
eval i = do
  case i of
    Set r v  -> modInstr pure       r v
    Add r v  -> modInstr (+)        r v
    Sub r v  -> modInstr subtract   r v
    Mul r v  -> modInstr (*)        r v
    Jump a b -> jump a b
  where
    succInstr = modify' $ \s -> s { instrPtr = succ (instrPtr s) }
    modInstr f r v = do
      m <- gets registers
      let x = value m v
      modify' $ \s -> s { registers = modifyRegister r (f x) m }
      succInstr
    jump a b = do
      m <- gets registers
      let x = value m a
      if x == 0
         then succInstr
         else let y = value m b
               in modify' $ \s -> s { instrPtr = y + instrPtr s }

examplePrg = Text.unlines
  ["set a 100"
  ,"add b 2"
  ,"mul b a"
  ,"sub a 1"
  ,"jnz a -2"
  ]


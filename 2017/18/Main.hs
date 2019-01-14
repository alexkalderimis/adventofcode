{-# LANGUAGE OverloadedStrings #-}

import           Data.Attoparsec.Text    (decimal, letter, signed)
import qualified Data.IntMap.Strict      as M
import           Data.Monoid
import qualified Data.Text               as Text
import           Data.Vector             (Vector)
import qualified Data.Vector             as V
import           Text.Parser.Char        (newline)
import           Text.Parser.Combinators (choice, sepBy1)

import           Elves
import           Elves.Advent

type Freq = Int
type Register = Char
type Value = Either Register Freq

data Instr
 = Play Value
 | Set Register Value
 | Add Register Value
 | Mul Register Value
 | Mod Register Value
 | Recover Value
 | Jump Value Value
 deriving (Show, Eq)

type Memory = M.IntMap Int
type Duet = Vector Instr

main :: IO ()
main = day 18 parser pt1 pt2 test
  where
    parser = (instrP `sepBy1` newline) <* many newline
    pt1 duet = print (runUntilRecovery $ V.fromList duet)
    pt2 _ = error "unimplemented"

test = do
  describe "example" $ do
    let mduet = parseOnly (instrP `sepBy1` newline) exampleDuet
    describe "runUntilRecovery" $ do
      it "gets the example correct" $ do
        let (Right duet) = mduet
        fst (runUntilRecovery (V.fromList duet)) `shouldBe` Just 4
    describe "instrP" $ do
      it "parses the example correctly" $ do
        mduet `shouldBe` Right [Set 'a' (Right 1)
                               ,Add 'a' (Right 2)
                               ,Mul 'a' (Left 'a')
                               ,Mod 'a' (Right 5)
                               ,Play (Left 'a')
                               ,Set 'a' (Right 0)
                               ,Recover (Left 'a')
                               ,Jump (Left 'a') (Right (-1))
                               ,Set 'a' (Right 1)
                               ,Jump (Left 'a') (Right (-2))
                               ]

instrP = choice ["snd " *> (Play <$> val)
                ,"rcv " *> (Recover <$> val)
                ,"set " *> (Set <$> reg <*> (" " *> val))
                ,"add " *> (Add <$> reg <*> (" " *> val))
                ,"mul " *> (Mul <$> reg <*> (" " *> val))
                ,"mod " *> (Mod <$> reg <*> (" " *> val))
                ,"jgz " *> (Jump <$> val <*> (" " *> val))
                ]
  where
    reg = letter
    val = choice [ Left <$> reg
                 , Right <$> signed decimal
                 ]

readRegister :: Register -> Memory -> Freq
readRegister c = maybe 0 id . M.lookup (fromEnum c)

modifyRegister :: Register -> (Freq -> Freq) -> Memory -> Memory
modifyRegister c f = M.alter (pure . f . maybe 0 id) (fromEnum c)

value :: Memory -> Value -> Freq
value m = either (`readRegister` m) id

runUntilRecovery :: Duet -> (Maybe Freq, Memory)
runUntilRecovery is = go Nothing mempty 0
  where
    go p m i = case is V.!? i of
                 Nothing -> (p,m)
                 Just instr -> case instr of
                   Recover v -> if value m v > 0 then (p,m) else go p m (succ i)
                   Play v    -> go (Just $ value m v) m (succ i)
                   Set r v   -> let x = value m v in go p (modifyRegister r (pure x) m) (succ i)
                   Add r v   -> let x = value m v in go p (modifyRegister r (+ x) m) (succ i)
                   Mul r v   -> let x = value m v in go p (modifyRegister r (* x) m) (succ i)
                   Mod r v   -> let x = value m v in go p (modifyRegister r (`mod` x) m) (succ i)
                   Jump a b  -> let i' = if value m a > 0 then i + value m b else succ i in go p m i'

exampleDuet = Text.unlines
  ["set a 1"
  ,"add a 2"
  ,"mul a a"
  ,"mod a 5"
  ,"snd a"
  ,"set a 0"
  ,"rcv a"
  ,"jgz a -1"
  ,"set a 1"
  ,"jgz a -2"
  ]

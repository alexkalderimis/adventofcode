{-# LANGUAGE OverloadedStrings #-}

import           Data.Attoparsec.Text    (decimal, letter, signed)
import           Data.Coerce             (coerce)
import qualified Data.Foldable           as F
import qualified Data.IntMap.Strict      as M
import           Data.Monoid
import           Data.Sequence           (Seq (..), (|>), (<|))
import qualified Data.Sequence           as Seq
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
 = Send Value
 | Set Register Value
 | Add Register Value
 | Mul Register Value
 | Mod Register Value
 | Receive Register
 | Jump Value Value
 deriving (Show, Eq)

type Memory = M.IntMap Freq
type Duet = Vector Instr

type Messages = Seq Freq
data Mailbox = Mailbox { outbox :: Messages, inbox :: Messages }
  deriving (Show, Eq)

newtype ThreadId = ThreadId Int deriving (Eq, Show)

data ThreadState = Yield Messages Int Memory -- this thread has just issued a send. Wake up all blocked threads
                 | Blocked Int Memory        -- this thread is blocked on receive
                 | Terminated Memory         -- this thread has run to conclusion, and may not be run again
                 deriving (Show, Eq) 

type RunnableThread = (ThreadId,Messages,Int,Memory)

type Threads = Seq (ThreadId, ThreadState)

data DuetResult = DuetResult { sentCount :: Int, threadState :: ThreadState }
  deriving (Show, Eq)

-- get the state of the first runnable thread
getRunnable :: Threads -> Maybe (RunnableThread, Threads)
getRunnable Empty = Nothing
getRunnable (a :<| threads) = case a of
  (tid, Yield inbox i mem) -> pure ((tid,inbox,i,mem),threads)
  _                        -> fmap (a <|) <$> getRunnable threads

-- try to unblock any blocked threads. Only unblock if
-- there are actually some messages to receive.
unblock :: ThreadId -> Messages -> Threads -> Threads
unblock _ Empty = id
unblock tid msgs = fmap $ \(tid', ts) ->
  (,) tid' $ if tid == tid'
     then case ts of Yield inbox i m -> Yield (inbox <> msgs) i m
                     Blocked i m     -> Yield msgs i m
                     _               -> ts
     else ts

getMessage :: Mailbox -> Maybe (Int, Mailbox)
getMessage m = case inbox m of
  (a :<| msgs) -> Just (a, m { inbox = msgs })
  Empty        -> Nothing

sendMessage :: Int -> Mailbox -> Mailbox
sendMessage a m = m { outbox = outbox m :|> a }

main :: IO ()
main = day 18 parser pt1 pt2 test
  where
    parser = (instrP `sepBy1` newline) <* many newline
    pt1 duet = print (runUntilRecovery $ V.fromList duet)
    pt2 duet = print (runDuet $ V.fromList duet)

test = do
  let parse = parseOnly (instrP `sepBy1` newline)
  describe "runDuet" $ do
    let mduet = parse exampleDeadlock
    it "produces the correct result" $ do
      let (Right duet) = V.fromList <$> mduet
          common = M.fromList [(fromEnum 'a', 1),(fromEnum 'b', 2)]
          expected = M.fromList [(0, DuetResult 3 (Blocked 6 (common <> M.fromList [(fromEnum 'p', 0), (fromEnum 'c', 1)])))
                                ,(1, DuetResult 3 (Blocked 6 (common <> M.fromList [(fromEnum 'p', 1), (fromEnum 'c', 0)])))
                                ]
      runDuet duet `shouldBe` expected
  describe "example" $ do
    let mduet = parse exampleDuet
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
                               ,Send (Left 'a')
                               ,Set 'a' (Right 0)
                               ,Receive 'a'
                               ,Jump (Left 'a') (Right (-1))
                               ,Set 'a' (Right 1)
                               ,Jump (Left 'a') (Right (-2))
                               ]

instrP = choice ["snd " *> (Send <$> val)
                ,"rcv " *> (Receive <$> reg)
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

-- run the duet using cooperative multi-tasking. Single threaded execution, with blocking
-- receive.
runDuet :: Duet -> M.IntMap DuetResult
runDuet is = go (M.fromList [(0,0),(1,0)]) init
  where
    init = Seq.fromList [(ThreadId tid, Yield mempty 0 (M.singleton (fromEnum 'p') tid)) | tid <- [0,1]]
    go sent waiting = case getRunnable waiting of
      Nothing -> let states = M.fromList $ coerce $ F.toList waiting
                  in zipMap DuetResult sent states
      Just ((ThreadId tid, inbox, i, mem),waiting') ->
        let (outbox,ts) = runThread is (Mailbox mempty inbox) mem i
            receiver    = ThreadId (succ tid `mod` Seq.length init)
         in go (M.adjust (+ Seq.length outbox) tid sent)
               (unblock receiver outbox (waiting' |> (ThreadId tid, ts)))

-- the evaluator for pt2. Runs a thread until termination, send or receive.
runThread :: Duet -> Mailbox -> Memory -> Int -> (Messages, ThreadState)
runThread is = go
  where
    go mail m i =
      let modInstr f r v = let x = value m v in go mail (modifyRegister r (f x) m) (succ i)
       in case is V.!? i of
                 Nothing -> (outbox mail, Terminated m)
                 Just instr -> case instr of
                   Set r v   -> modInstr pure       r v
                   Add r v   -> modInstr (+)        r v
                   Mul r v   -> modInstr (*)        r v
                   Mod r v   -> modInstr (flip mod) r v
                   Jump a b  -> let i' = if value m a > 0 then i + value m b else succ i
                                 in go mail m i'
                   Send v    -> let mail' = sendMessage (value m v) mail
                                 in (outbox mail', Yield (inbox mail) (succ i) m)
                   Receive r -> case getMessage mail of
                     Just (a,mail') -> go mail' (modifyRegister r (pure a) m) (succ i)
                     Nothing        -> (outbox mail, Blocked i m)

-- the evaluator for pt1. Differs in the sense of Send/Receive
runUntilRecovery :: Duet -> (Maybe Freq, Memory)
runUntilRecovery is = go Nothing mempty 0
  where
    go p m i =
      let modInstr f r v = let x = value m v in go p (modifyRegister r (f x) m) (succ i)
       in case is V.!? i of
                 Nothing -> (p,m)
                 Just instr -> case instr of
                   Receive r -> if readRegister r m > 0 then (p,m) else go p m (succ i)
                   Send v    -> go (Just $ value m v) m (succ i)
                   Set r v   -> modInstr pure       r v
                   Add r v   -> modInstr (+)        r v
                   Mul r v   -> modInstr (*)        r v
                   Mod r v   -> modInstr (flip mod) r v
                   Jump a b  -> let i' = if value m a > 0 then i + value m b else succ i in go p m i'

-- join a map on its keys
zipMap :: (a -> b -> c) -> M.IntMap a -> M.IntMap b -> M.IntMap c
zipMap f = M.mergeWithKey (\_key a b -> Just (f a b)) (pure mempty) (pure mempty)

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

exampleDeadlock = Text.unlines
  ["snd 1"
  ,"snd 2"
  ,"snd p"
  ,"rcv a"
  ,"rcv b"
  ,"rcv c"
  ,"rcv d"
  ]

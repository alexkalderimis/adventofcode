{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards          #-}

import           Control.Applicative
import           Data.Hashable           (Hashable)
import           Data.HashMap.Strict     (HashMap)
import qualified Data.HashMap.Strict     as M
import           Data.IntSet             (IntSet)
import qualified Data.IntSet             as S
import qualified Data.List               as L
import           Data.Maybe
import           Data.Monoid
import           Data.Tree               (Forest, Tree (..))
import           System.Exit
import Control.Monad.State.Strict
import Data.Functor.Identity

import           Data.Attoparsec.Text    ((<?>), decimal, letter)
import           Data.Text               (Text)
import qualified Data.Text               as Text
import           Text.Parser.Char        (newline, space, text)
import           Text.Parser.Combinators (choice, sepBy1)

import           Elves
import           Elves.Advent

data Bit        = Zero   | One deriving (Show, Eq, Ord, Bounded, Enum)
data Movement   = GoLeft | GoRight deriving (Show, Eq)
type Tape       = IntSet
newtype PhaseId = PhaseId Char deriving (Show, Eq, Hashable)

newtype TuringM a = TuringM { runTuringM :: StateT TuringState Identity a }
  deriving (Functor, Applicative, Monad, MonadState TuringState)

data TuringState = TuringState
  { tape :: Tape
  , cursor :: Int
  , turingMachine :: StateMachine
  } deriving (Show, Eq)

data StateMachine = StateMachine
  { machineHdr    :: Header
  , machinePhases :: HashMap PhaseId Phase
  } deriving (Show, Eq)

data Header = Header { initPhase :: PhaseId, checkSumAfter :: Word }
  deriving (Show, Eq)
data Phase = Phase { onZero :: Action, onOne :: Action }
  deriving (Show, Eq)
data Action = Action
  { actionWrite :: Bit
  , actionMove  :: Movement
  , actionCont  :: PhaseId
  } deriving (Show, Eq)

main :: IO ()
main = day 25 parser pt1 pt2 test
  where
    pt1 m = do
      let s = newState m
      print . checksum $ execState (runTuringM runUntilCheckSum) s
    pt2 = undefined

newState :: StateMachine -> TuringState
newState = TuringState mempty 0

checksum :: TuringState -> Int
checksum = S.size . tape

runUntilCheckSum :: TuringM PhaseId
runUntilCheckSum = do
  hdr <- gets (machineHdr . turingMachine)
  applyNM (fromIntegral $ checkSumAfter hdr) run (initPhase hdr)

run :: PhaseId -> TuringM PhaseId
run phaseId = do
  phase <- getPhase phaseId
  val <- readTape
  case val of
    Zero -> act (onZero phase)
    One  -> act (onOne  phase)

readTape :: TuringM Bit
readTape = do
  ix <- gets cursor
  t  <- gets tape
  return (if S.member ix t then One else Zero)

getPhase :: PhaseId -> TuringM Phase
getPhase pid = do
  phases <- gets (machinePhases . turingMachine)
  case M.lookup pid phases of
    Nothing -> fail $ "machine error - cannot find phase: " ++ show pid
    Just p -> pure p

act :: Action -> TuringM PhaseId
act Action{..} = do
  writeTape actionWrite
  move actionMove
  pure actionCont

move :: Movement -> TuringM ()
move m = do
  let f = case m of GoLeft -> pred
                    GoRight -> succ
  modify' $ \s -> s { cursor = f (cursor s) }

writeTape :: Bit -> TuringM ()
writeTape b = do
  ix <- gets cursor
  let f = case b of Zero -> S.delete ix
                    One  -> S.insert ix 
  modify' $ \s -> s { tape = f (tape s) }

test = do
  let em = parseOnly parser exampleMachine
  let es = fmap newState em
  describe "runUntilCheckSum" $ do
    it "runs to the correct state" $ do
      let (Right s) = es
          (pid, s') = runState (runTuringM runUntilCheckSum) s
      (pid, tape s') `shouldBe` (PhaseId 'A', S.fromList [-2,-1,1])
  describe "pt1" $ do
    it "produces the correct checksum" $ do
      let (Right s) = es
          cs = checksum $ execState (runTuringM runUntilCheckSum) s
      cs `shouldBe` 3
    
  describe "parser" $ do
    it "parses the example correctly" $ do
      let expected = StateMachine (Header (PhaseId 'A') 6) 
                       $ M.fromList
                         [(PhaseId 'A'
                          ,Phase (Action One GoRight (PhaseId 'B'))
                                 (Action Zero GoLeft (PhaseId 'B'))
                          )
                         ,(PhaseId 'B'
                          ,Phase (Action One GoLeft  (PhaseId 'A'))
                                 (Action One GoRight (PhaseId 'A'))
                          )
                         ]
      em `shouldBe` Right expected

parser :: Parser StateMachine
parser = StateMachine <$> (header <?> "header")
                      <*> (many newline *> phases <?> "phases")
  where
    header = Header <$> (text "Begin in state "
                         *> phaseId <* "." <* newline)
                    <*> (text "Perform a diagnostic checksum after "
                         *> decimal <* " steps.")
    phases = fmap M.fromList ((phase <?> "phase") `sepBy1` some newline)
    phaseId = PhaseId <$> letter <?> "phaseId"
    phase = (,) <$> ("In state " *> phaseId <* ":")
                <*> liftA2 Phase (action "0" <?> "action-0")
                                 (action "1" <?> "action-1")
    action trigger = newline *> some space
                     *> "If the current value is "
                     *> text trigger
                     *> ":"
                     *> (Action <$> bit <*> move <*> cont)
    bit = "Write the value " `actionLine` choice [Zero <$ "0", One <$ "1"]
    move = "Move one slot to the " `actionLine` choice [GoLeft <$ "left"
                                                       ,GoRight <$ "right"
                                                       ]
    cont = actionLine "Continue with state " phaseId
    actionLine :: Text -> Parser a -> Parser a
    actionLine t p = (newline *> some space *> "- " *> text t) *> p <* "."

exampleMachine = Text.unlines
  ["Begin in state A."
  ,"Perform a diagnostic checksum after 6 steps."
  ,""
  ,"In state A:"
  ,"  If the current value is 0:"
  ,"    - Write the value 1."
  ,"    - Move one slot to the right."
  ,"    - Continue with state B."
  ,"  If the current value is 1:"
  ,"    - Write the value 0."
  ,"    - Move one slot to the left."
  ,"    - Continue with state B."
  ,""
  ,"In state B:"
  ,"  If the current value is 0:"
  ,"    - Write the value 1."
  ,"    - Move one slot to the left."
  ,"    - Continue with state A."
  ,"  If the current value is 1:"
  ,"    - Write the value 1."
  ,"    - Move one slot to the right."
  ,"    - Continue with state A."
  ]

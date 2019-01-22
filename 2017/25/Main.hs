{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}

import           Control.Applicative
import           Control.Monad.State.Strict
import           Data.Functor.Identity
import           Data.Hashable              (Hashable)
import           Data.HashMap.Strict        (HashMap)
import qualified Data.HashMap.Strict        as M
import qualified Data.HashSet               as HS
import           Data.IntSet                (IntSet)
import qualified Data.IntSet                as S

import           Data.Attoparsec.Text       (decimal, letter, (<?>))
import           Data.Text                  (Text)
import qualified Data.Text                  as Text
import           Text.Parser.Char           (newline, space, text)
import           Text.Parser.Combinators    (choice, sepBy1)

import           Elves
import           Elves.Advent

data Bit        = Zero   | One deriving (Show, Eq, Ord, Bounded, Enum)
data Movement   = GoLeft | GoRight deriving (Show, Eq)
type Tape       = IntSet -- only represent 1's
newtype PhaseId = PhaseId Char deriving (Show, Eq, Hashable)

newtype TuringM a = TuringM { runTuringM :: StateT TuringState Identity a }
  deriving (Functor, Applicative, Monad, MonadState TuringState)

data TuringState = TuringState
  { tape   :: !Tape
  , cursor :: !Int
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
main = day 25 (parser >>= validateMachine) pt1 pt2 test
  where
    pt1   = print . checksum . snd . runUntilCheckSum
    pt2 _ = putStrLn "woot!"

newState :: TuringState
newState = TuringState mempty 0

checksum :: TuringState -> Int
checksum = S.size . tape

runUntilCheckSum :: StateMachine -> (PhaseId, TuringState)
runUntilCheckSum m = runState (runTuringM go) newState
  where go = applyNM (fromIntegral $ checkSumAfter $ machineHdr m)
                     (run $ machinePhases m)
                     (initPhase $ machineHdr m)

validateMachine :: (Monad m) => StateMachine -> m StateMachine
validateMachine m@StateMachine{..}
  | not (HS.null missingPhases) = fail $ "invalid machine, missing: " ++ show missingPhases
  | otherwise = pure m
  where
    phases = initPhase machineHdr : fmap actionCont (M.elems machinePhases >>= actions)
    actions p = [onZero p, onOne p]
    missingPhases = HS.filter (not . flip M.member machinePhases) (HS.fromList phases)

run :: HashMap PhaseId Phase -> PhaseId -> TuringM PhaseId
run phases phaseId = do
  let Phase{..} = phases M.! phaseId
  val           <- gets readTape
  case val of
    Zero -> act onZero
    One  -> act onOne

act :: Action -> TuringM PhaseId
act Action{..} = do
  modify' (writeTape actionWrite)
  modify' (move actionMove)
  pure actionCont

move :: Movement -> TuringState -> TuringState
move m s = s { cursor = f m (cursor s) }
  where f GoLeft  = pred
        f GoRight = succ

readTape :: TuringState -> Bit
readTape (TuringState t ix) = if S.member ix t then One else Zero

writeTape :: Bit -> TuringState -> TuringState
writeTape b s = s { tape = setBit b (cursor s) (tape s) }

setBit :: Bit -> Int -> Tape -> Tape
setBit Zero = S.delete
setBit One  = S.insert

test = do
  let em = parseOnly parser exampleMachine
  describe "validateMachine" $ do
    specify "the example is valid" $ do
      let (Right m) = em
      m' <- validateMachine m
      m `shouldBe` m'
    specify "deleting a phase makes it invalid" $ do
      let (Right m) = em
          invalid = m { machinePhases = M.delete (PhaseId 'B') (machinePhases m) }
      validateMachine invalid `shouldThrow` anyException

  describe "runUntilCheckSum" $ do
    it "runs to the correct state" $ do
      let (Right m) = em
          (pid, s) = runUntilCheckSum m
      (pid, tape s) `shouldBe` (PhaseId 'A', S.fromList [-2,-1,1])
  describe "pt1" $ do
    it "produces the correct checksum" $ do
      let (Right m) = em
          cs = checksum . snd $ runUntilCheckSum m
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

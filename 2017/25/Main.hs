{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}

import           Control.Applicative
import           Data.Hashable           (Hashable)
import           Data.HashMap.Strict     (HashMap)
import qualified Data.HashMap.Strict     as M
import           Data.IntSet             (IntSet)
import qualified Data.List               as L
import           Data.Maybe
import           Data.Monoid
import           Data.Tree               (Forest, Tree (..))
import           System.Exit

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
main = day 24 parser pt1 pt2 test
  where
    pt1 = undefined
    pt2 = undefined

test = do
  describe "parser" $ do
    let em = parseOnly parser exampleMachine
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

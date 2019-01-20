{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}

import           Control.Lens                (modifying, _1, _2)
import           Control.Monad.State.Strict
import           Control.Monad.Writer.Strict
import           Data.Foldable               (traverse_)
import           Data.Functor.Identity
import           Data.HashMap.Strict         (HashMap)
import qualified Data.HashMap.Strict         as M
import           Data.HashSet                (HashSet)
import qualified Data.HashSet                as S
import           Data.Monoid
import           Data.Maybe

import           Control.Applicative
import           Data.Attoparsec.Text        (char)
import qualified Data.Text                   as Text
import           Text.Parser.Char            (newline)
import           Text.Parser.Combinators     (sepBy1)

import           Elves
import           Elves.Advent
import           Elves.Cartesian             (Direction (..), Location, move)

-- the action here has two different kinds of action,
-- one that takes place over a Set, the other that takes
-- place over a Map. This type is parameterised to allow
-- both to be expressed with the same logic.
--
-- The writer state is Endo [Command] so that we get right-associative appends
-- (see: https://kseo.github.io/posts/2017-01-21-writer-monad.html for a good
-- explanation).
newtype SporificaM c a = Sporifica { runVirus :: WriterT (Endo [Command])
                                                         (StateT (Virus, c) Identity)
                                                         a
                                   }
  deriving (Functor, Applicative, Monad, MonadWriter (Endo [Command]), MonadState (Virus, c))

-- unwrap the transformers, and then apply the Endo [Command] with [] to get the
-- list of commands
runSporifica :: Virus -> c -> SporificaM c a -> (Virus, c, [Command], a)
runSporifica v c m = let ((a, w), (v', c')) = runIdentity . runStateT (runWriterT (runVirus m)) $ (v,c)
                      in (v', c', appEndo w [], a)

-- Defunctionalisation of the actions, with the bonus that we can inspect
-- the log of actions after the state has been run to detect infections,
-- and we can easily share logic between pt1 and pt2
data Command
  = TurnRight
  | TurnLeft
  | Infect
  | Clear -- only applicable for pt1
  | MoveForward
  | Weaken -- only applicable for pt2
  deriving (Show, Eq)

-- only states are infected/clear. We just record infected nodes
type Cluster         = HashSet Location 
-- here nodes may have any of the four node-states. We do not store
-- the locations of clear nodes - they are represented by absence in the
-- map.
type DetailedCluster = HashMap Location NodeState

data Virus = Virus
  { virusPosition  :: Location
  , virusDirection :: Direction
  } deriving (Show, Eq)

data NodeState = Clean | Weakened | Infected | Flagged
  deriving (Show, Eq, Bounded, Enum)

main :: IO ()
main = day 22 parser pt1 pt2 test
  where
    pt1 (c,bs) = print (doPt1 ten_thousand c bs)
    pt2 (c,bs) = print (doPt2 ten_million  c bs)

    ten_million = 10 * (10 ^ 6)
    ten_thousand = 10 * (10 ^ 3)

test = do
  let exampleCluster = Text.unlines ["..#"
                                    ,"#.."
                                    ,"..."
                                    ]
  describe "pt2" $ do
    let mr = parseOnly parser exampleCluster
        infections n = uncurry (doPt2 n) <$> mr
    specify "after 100 iterations, 26 infections will have occurred" $ do
      infections 100 `shouldBe` Right 26
    specify "after 10M iterations, 2511944 infections will have occurred" $ do
      infections (10 * (10 ^ 6)) `shouldBe` Right 2511944

  describe "pt1" $ do
    describe "example" $ do
      let origin = (0,0)
          c = S.fromList $ fmap ($ origin) [move West 1, move North 1 . move East 1]
          v = Virus origin North
      specify "after 70 bursts, there have been 41 infections" $ do
        let action = applyNM 70 (pure play1) ()
            (_, _, cmds, _) = runSporifica v c action
            infections = sum [1 | Infect <- cmds]
        infections `shouldBe` 41
    describe "putting it all together" $ do
      let mr = parseOnly parser exampleCluster
          infections n = uncurry (doPt1 n) <$> mr
      it "has the correct result after 70 iterations" $ do
        infections 70 `shouldBe` Right 41
      it "has the correct result after 10000 iterations" $ do
        infections 10000 `shouldBe` Right 5587
  describe "midPoint" $ do
    it "gets the midPoint of the example correct" $ do
      midPoint ((0,0),(2,2)) `shouldBe` (1,1)
  describe "parsing" $ do
    let mr = parseOnly parser exampleCluster
    it "parses the example correctly" $ do
      mr `shouldBe` Right (S.fromList [(0,2),(1,0)], ((0,0),(2,2)))

doPt :: SporificaM c () -> (Cluster -> c) -> Int -> Cluster -> (Location,Location) -> Int
doPt act fromCluster n c bs = do
  let p = midPoint bs
      s = fromCluster c
      action = applyNM n (pure act) ()
      v = Virus p North
      (_, _, cmds, _) = runSporifica v s action
   in sum [1 | Infect <- cmds]

doPt1 :: Int -> Cluster -> (Location,Location) -> Int
doPt1 = doPt play1 id

doPt2 :: Int -> Cluster -> (Location,Location) -> Int
doPt2 = doPt play2 fromCluster

fromCluster :: Cluster -> DetailedCluster
fromCluster = M.map (const Infected) . S.toMap

midPoint :: (Location,Location) -> Location
midPoint ((y,x),(y',x')) = ((y + y') `div` 2, (x + x') `div` 2)

parser :: Parser (Cluster, (Location, Location))
parser = do
  rows <- zip [0 ..] <$> (rowP `sepBy1` newline)
  let c = S.fromList [(y,x) | (y,row) <- rows
                            , (x,'#') <- row
                     ]
      bs = ((0,0), (length rows - 1, maximum (length . snd <$> rows) - 1))
  return (c, bs)
  where
    rowP = zip [0 ..] <$> some (char '.' <|> char '#')

play :: (Virus -> c -> [Command]) -> (Command -> SporificaM c ()) -> SporificaM c ()
play burst execute = do
  (v,c) <- get
  let commands = burst v c
  recordCommands commands
  traverse_ execute commands

play1 :: SporificaM Cluster ()
play1 = play burst1 execute1

play2 :: SporificaM DetailedCluster ()
play2 = play burst2 execute2

recordCommands :: [Command] -> SporificaM a ()
recordCommands cs = tell (Endo (cs <>))

execute1 :: Command -> SporificaM Cluster ()
execute1 TurnRight   = modifying _1 turnRight
execute1 TurnLeft    = modifying _1 turnLeft
execute1 MoveForward = modifying _1 moveForward
execute1 Infect      = gets fst >>= modifying _2 . infect
execute1 Clear       = gets fst >>= modifying _2 . clear
execute1 cmd         = error $ "Cannot handle command: " ++ show cmd

burst1 :: Virus -> Cluster -> [Command]
burst1 virus cluster = case S.member (virusPosition virus) cluster of
  True  -> [TurnRight, Clear, MoveForward]
  False -> [TurnLeft, Infect, MoveForward]

execute2 :: Command -> SporificaM DetailedCluster ()
execute2 TurnRight   = modifying _1 turnRight
execute2 TurnLeft    = modifying _1 turnLeft
execute2 MoveForward = modifying _1 moveForward
execute2 Infect      = gets fst >>= modifying _2 . weaken -- special case
execute2 Weaken      = gets fst >>= modifying _2 . weaken
execute2 cmd         = error $ "Cannot handle command: " ++ show cmd

burst2 :: Virus -> DetailedCluster -> [Command]
burst2 virus cluster = case M.lookup (virusPosition virus) cluster of
  Nothing       -> [TurnLeft, Weaken, MoveForward]
  Just Weakened -> [Infect, MoveForward] -- we issue an Infect command so that it can be counted.
  Just Infected -> [TurnRight, Weaken, MoveForward]
  Just Flagged  -> [TurnRight, TurnRight, Weaken, MoveForward]
  Just Clean    -> error "Clean node in map" -- should not occur

-- The actions on the state components, used by the execute family of functions:

turnLeft :: Virus -> Virus
turnLeft v = v { virusDirection = cyclePred (virusDirection v) }

turnRight :: Virus -> Virus
turnRight v = v { virusDirection = cycleSucc (virusDirection v) }

moveForward :: Virus -> Virus
moveForward v = v { virusPosition = move (virusDirection v) 1 (virusPosition v) }

infect :: Virus -> Cluster -> Cluster
infect v = S.insert (virusPosition v)

clear :: Virus -> Cluster -> Cluster
clear v = S.delete (virusPosition v)

-- weaken handles all node-state transitions, including infection.
weaken :: Virus -> DetailedCluster -> DetailedCluster
weaken v = flip M.alter (virusPosition v) $ \mv -> 
  case cycleSucc (fromMaybe Clean mv) of Clean -> Nothing
                                         nstate -> Just nstate


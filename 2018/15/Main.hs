{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NumericUnderscores #-}

import Control.Arrow
import Control.Monad.Extra
import Control.Applicative
import GHC.Generics (Generic)
import Data.Hashable
import Data.Functor
import qualified Data.Array as A
import qualified Data.Foldable as F
import qualified Data.Map.Strict as M
import qualified Data.HashSet as HS
import Control.Lens
import Data.Monoid
import Data.Maybe
import Data.Bool
import Safe
import Text.Printf
import Control.Monad.State.Strict
import qualified Data.List.Extra as L
import           Text.Parser.Char (text)
import qualified Data.Text as T
import qualified Data.Time.Clock      as Clock
import           Data.Fixed (Fixed(..))
import           Data.Text (Text)
import Control.Parallel.Strategies (parMap, parList, rdeepseq, using)

import Test.QuickCheck hiding (Result)

import           Elves
import           Elves.Ord
import           Elves.Advent
import qualified Elves.AStar as AStar
import qualified Elves.Clique as Clique
import qualified Elves.StrictGrid as G
import           Elves.Coord.Strict
import qualified Elves.Coord as Coord

type Location = Coordinate
type Distance = Int
type Dungeon = A.Array Location Feature
type Creatures = M.Map Location Creature

data Feature    = Wall | Open deriving (Show, Eq)
data Allegiance = Elf | Goblin deriving (Show, Eq, Generic)

instance Hashable Allegiance

newtype CreatureId = CreatureId Int deriving (Show, Eq, Hashable)

data Creature = Creature
  { _alliegance :: !Allegiance
  , _hp :: !Int
  , _creatureId :: !CreatureId
  } deriving (Show, Eq)

makeLenses ''Creature

newtype Game a = Game { _runGame :: State GameState a }
  deriving (Functor, Applicative, Monad, MonadState GameState)

instance Semigroup a => Semigroup (Game a) where
  a <> b = (<>) <$> a <*> b

instance Monoid a => Monoid (Game a) where
  mempty = pure mempty

data GameState = GameState
  { _dungeon   :: !Dungeon
  , _creatures :: !Creatures
  , _rounds   :: {-# UNPACK #-} !Int
  , _killed   :: !(HS.HashSet CreatureId)
  , _moved    :: {-# UNPACK #-} !Int
  , _alive    :: {-# UNPACK #-} !(Int, Int)
  , _elfPower :: {-# UNPACK #-} !Int
  } deriving (Show)

makeLenses ''GameState

aliveLens Goblin = remainingGoblins
aliveLens Elf    = remainingElves

remainingElves, remainingGoblins :: Lens' GameState Int
remainingElves   = alive . _1
remainingGoblins = alive . _2

main :: IO ()
main = day 15 parseInput pt1 pt2 spec
  where
    pt1 gs = sim gs >>= showOutcome
    pt2 gs = case minElfPower gs of
               Nothing -> putStrLn "Unwinnable :("
               Just (ep, r) -> do
                 putStrLn $ "MIN ELF POWER: " <> show ep
                 showOutcome r

    showOutcome r = putStrLn $ "OUTCOME: " <> show (fullRounds r * totalHP r)

play :: Game a -> GameState -> a
play game = evalState (_runGame game)

runGame :: Game a -> GameState -> GameState
runGame g = execState (_runGame g)

currentCreatures :: Traversal' GameState Creature
currentCreatures f gs = 
  let cs = gs ^. creatures & M.toAscList
      fcs = traverse (\(i,c) -> fmap (i,) (f c)) cs
   in fmap (\cs -> gs & creatures .~ M.fromAscList cs) fcs

winner :: GameState -> Maybe (Allegiance, Int)
winner gs = case gs ^. alive of
  (0, n) -> Just (Goblin, n)
  (n, 0) -> Just (Elf, n)
  _ -> Nothing

-- all units have the same attack power
attackPower :: Int
attackPower = 3

-- returns distance, along with the next step to take
distance :: GameState -> Location -> Location -> Maybe (Int, Location)
distance gs origin destination
  | origin == destination        = Just (0, origin)
  | neighbour origin destination = Just (0, origin) -- no steps necessary, within attack range
  | otherwise = join $ lowest NullsLast (dist <$> ns)
  where
    dist p = let mpath = AStar.aStar neighbours
                                     cost
                                     (Coord.manhattan destination)
                                     (== destination)
                                     p
              in fmap (\path -> (length path, p)) mpath
                   
    cost :: a -> a -> Int
    cost b a = 1

    neighbours = openSquares (gs & creatures %~ M.delete destination)
    ns = neighbours origin

-- immediately adjacent neighbours, in reading order.
nwes :: Location -> [Location]
nwes x = Coord.translate x <$>
             [ Coord (-1) 0
             , Coord 0 (-1)
             , Coord 0 1
             , Coord 1 0
             ]

neighbour :: Location -> Location -> Bool
neighbour a b = Coord.manhattan a b == 1

game :: Game Result
game = untilJustM (unlessM gameOver playRound >> gets result)

gameOver :: Game Bool
gameOver = uses alive (anyOf both (== 0))

playRound :: Game ()
playRound = creaturesToPlay >>= foldMap playNextCreature >>= incRoundCounter
  where
    playNextCreature (loc, c) = ifM (allTargetsAreDead c)
                                    (pure (All False))
                                    (All True <$ takeTurn loc c)

incRoundCounter :: All -> Game ()
incRoundCounter fullRound = when (getAll fullRound) (rounds #%= succ)

allTargetsAreDead :: Creature -> Game Bool
allTargetsAreDead c = uses (aliveLens (target c)) (== 0)

creaturesToPlay :: Game [(Location, Creature)]
creaturesToPlay = do
  moved #%= pure 0
  uses creatures M.toAscList

-- find the closest creature of a given alliegance (if reachable) returning
-- the distance, the destination, and the first step we need to take to get there.
--
-- If two enemies are closest, prefer the one in minimum reading order.
findClosest :: Allegiance -> Location -> Game (Maybe (Distance, Location, Location))
findClosest side loc = minimumMay <$> liftA2 distances enemyLocations (gets distance)
  where
    distances locations d = [ (dist, dest, step) | dest <- locations
                                                 , Just (dist, step) <- [d loc dest]
                                                 ]
                            `using`
                            parList rdeepseq

    enemyLocations = uses creatures (M.keys . M.filter (isA side))

target :: Creature -> Allegiance
target c = case c ^. alliegance of
             Elf -> Goblin
             Goblin -> Elf

takeTurn :: HasCallStack => Location -> Creature -> Game ()
takeTurn pos c = unlessM (isDead c) $ do
  let enemies = target c
  whenJustM (findClosest enemies pos) $ \(d, t, step) -> do
    when (d > 0) (takeStep c pos step)
    attack enemies t step

isDead :: Creature -> Game Bool
isDead c = uses killed (HS.member (c ^. creatureId))

sumHP :: GameState -> Int
sumHP s = sum (s ^.. currentCreatures.hp)

takeStep :: HasCallStack => Creature -> Location -> Location -> Game ()
takeStep c current step = do
  creatures #%= (M.insert step c . M.delete current)
  moved #%= succ

openSquares :: GameState -> Location -> [Location]
openSquares gs = filter canBeMovedTo . nwes
  where
    canBeMovedTo = allPreds [inBounds (gs^.dungeon), isOpen (gs^.dungeon), unoccupied]
    unoccupied = not . flip M.member (gs ^. creatures)

isOpen :: Dungeon -> Location -> Bool
isOpen d = (Open ==) . (d A.!)

inBounds :: Dungeon -> Location -> Bool
inBounds d = A.inRange (A.bounds d)
  
attack :: Allegiance -> Location -> Location -> Game ()
attack _ enemy self | not (neighbour enemy self) = pure ()
attack side enemy self = do
  -- the foe at enemy is the closest, but might not be the best target. Now that we are
  -- up close and personal, also take attackOrder into account.
  (loc, foe) <- uses creatures (\m -> fromMaybe (enemy, m M.! enemy) (nextTarget side self m))
  creatures #%= M.delete loc
  power <- case side of Elf -> pure attackPower
                        Goblin -> use elfPower
  let injured = foe & hp -~ power
  if dead injured
    then do killed #%= HS.insert (injured ^. creatureId)
            aliveLens (injured ^. alliegance) #%= pred
    else creatures #%= M.insert loc injured

nextTarget :: Allegiance -> Location -> Creatures -> Maybe (Location, Creature)
nextTarget side loc cs
  = lowest attackOrder
  . filter (isA side . snd)
  . mapMaybe (\l -> (l,) <$> M.lookup l cs)
  $ nwes loc

dead :: Creature -> Bool
dead = (<= 0) . view hp

isA :: Allegiance -> Creature -> Bool
isA side = (side ==) . _alliegance

-- prefer the weakest creature, tie breaking on row order.
attackOrder :: (Coordinate, Creature) -> (Int, Coordinate)
attackOrder (loc, creature) = (_hp creature, loc)

parseInput :: Parser GameState
parseInput = do
  board <- G.gridP cellP
  let dng = fmap fst board
      cs  = zipWith (\i -> second (creatureId .~ CreatureId i)) [0..]
          . mapMaybe (\(loc, mc) -> (loc,) <$> mc)
          . A.assocs
          $ fmap snd board

  pure $ dungeonState dng cs

dungeonState :: Dungeon -> [(Location, Creature)] -> GameState
dungeonState dng cs = let m = M.fromList $ zipWith (second . set creatureId . CreatureId) [0..] cs
                      in GameState dng m 0 HS.empty 0 (0,0) attackPower
                          & remainingElves   .~ count (isA Elf . snd) cs
                          & remainingGoblins .~ count (isA Goblin . snd) cs

cellP :: Parser (Feature, Maybe Creature)
cellP =   text "#" $> (Wall, Nothing)
      <|> text "." $> (Open, Nothing)
      <|> text "E" $> (Open, mkCreature Elf initialHP)
      <|> text "G" $> (Open, mkCreature Goblin initialHP)

mkCreature :: Applicative f => Allegiance -> Int -> f Creature
mkCreature a hp = pure $ Creature a hp (CreatureId 0)

initialHP :: Int
initialHP = 200

showGameState :: Bool -> GameState -> String
showGameState withCs gs
  = unlines [ mconcat ["After ", show (gs ^. rounds), " full rounds:"]
            , G.draw (if withCs then annotated else tiles)
            ]
  where
    annotated = let d = gs^.dungeon
                    summaries = [ (r, summary)
                                | rowg <- L.groupOn (row . fst) $ L.sortOn (row . fst) cs
                                , let r = row . fst . head $ rowg
                                , let summary = L.intercalate ", " . fmap (showCreature . snd) $ rowg
                                ]
                    extraWidth = 2 + maximum (length . snd <$> summaries)
                    offset     = 2 + col (snd $ A.bounds d)
                    bs = let (lb, ub) = A.bounds d
                          in (lb, Coord.translate (Coord 0 (Col extraWidth)) ub)
                 in A.listArray bs (repeat ' ')
                    A.// A.assocs tiles
                    A.// [ (Coord r (offset + c), chr) | (r, summary) <- summaries
                                                       , (c, chr) <- zip [0..] summary
                         ]

    tiles           = fmap tile (gs^.dungeon) A.// creatureOverlay
    creatureOverlay = second (showSide . view alliegance) <$> cs

    showCreature c = concat [pure (showSide (c ^. alliegance))
                            ,"("
                            ,show (c ^. hp)
                            ,")"
                            ]

    cs = gs ^. creatures & M.toList

    tile Wall = '#'
    tile Open = '.'
    showSide Elf = 'E'
    showSide Goblin = 'G'

exampleOne :: Text
exampleOne = T.unlines
  ["#######"
  ,"#E..G.#"
  ,"#...#.#"
  ,"#.G.#G#"
  ,"#######"
  ]
                      
exampleTwo :: Text
exampleTwo = T.unlines
  ["#########"
  ,"#G..G..G#"
  ,"#.......#"
  ,"#.......#"
  ,"#G..E..G#"
  ,"#.......#"
  ,"#.......#"
  ,"#G..G..G#"
  ,"#########"
  ]

exampleThree :: Text
exampleThree = T.unlines
  ["#######"
  ,"#.G...#"
  ,"#...EG#"
  ,"#.#.#G#"
  ,"#..G#E#"
  ,"#.....#"
  ,"#######"
  ]

data Result = Result { wonBy :: Allegiance
                     , totalHP :: Int
                     , fullRounds :: Int
                     }
  deriving (Show, Eq)

result :: GameState -> Maybe Result
result gs = Result <$> (fst <$> winner gs)
                   <*> pure (sum (gs ^.. currentCreatures.hp))
                   <*> pure (gs ^. rounds)

sim :: GameState -> IO Result
sim = go []
  where
    go states gs = do
      start <- Clock.getCurrentTime
      let gs' = runGame playRound gs
      putStrLn (showGameState True gs')
      end <- Clock.getCurrentTime
      let elapsed = Clock.diffUTCTime end start
      case result gs' of
        Just r -> r <$ forM_ (reverse (withNext ((gs', elapsed) : states))) (\((s,t), ms) ->
                    printf "Round #%02d (%3dms): movement %-2d | damage %-2d | killed %-2d\n"
                           (view rounds s)
                           (case Clock.nominalDiffTimeToSeconds t of
                             MkFixed ps -> ps `div` 1_000_000_000)
                           (view moved s)
                           (case ms of
                             Nothing -> 0
                             Just (prev,_) -> sumHP prev - sumHP s)
                           (case ms of
                             Nothing -> 0
                             Just (prev,_) -> sum (prev ^.. alive.both) - sum (s ^.. alive.both)))
        Nothing -> go ((gs, elapsed) : states) gs'

-- binary search to find the lowest possible power where
-- we are guaranteed not to lose any elves.
--
-- returns the powerlevel, and the result of the game
minElfPower :: GameState -> Maybe (Int, Result)
minElfPower gs = go (attackPower + 1, initialHP) Nothing
  where 
    e0 = gs ^. remainingElves
    withPower p = do let s = runGame game gs { _elfPower = p }
                     guard (e0 == s ^. remainingElves)
                     pure s
    go rng mr = case A.rangeSize rng of
      0 -> Nothing
      1 -> let p = fst rng
               r = mr <|> (withPower p >>= result)
            in (p,) <$> r
      _ -> let p = uncurry Coord.midpoint rng
            in case withPower p of
                 Just s -> go (fst rng, p) (result s)
                 Nothing -> go (p + 1, snd rng) Nothing

-- TODO:
-- properties to verify:
--
-- given two viable routes of equal length, we prefer to step NWES
-- given two enemies at equal distance, we prefer the one at the lower reading position
-- we don't move if within striking distance
spec :: Spec
spec = do
  describe "findClosest" $ do
    let Right gs = parseOnly parseInput
                 $ T.unlines ["#######"
                             ,"#.EGG.#"
                             ,"#.GGG.#"
                             ,"#.GGG.#"
                             ,"#.....#"
                             ,"#######"
                             ]
    let findElf = play (findClosest Elf (Coord 2 3))
        elf = Coord 1 2

    context "boxed-in" $ do
      it "should return nothing" $ do
        let mc = findElf gs

        mc `shouldBe` Nothing

      it "should return the elf when unboxed" $ do
        let blocker = Coord 2 4
        let gs' = gs & creatures #%~ M.delete blocker
        let mc = findElf gs'

        mc `shouldBe` Just (11, elf, blocker)

    it "can find the elf for all other goblins" $ do
      let rs = fmap (\loc -> play (findClosest Elf loc) gs)
                    (gs ^. creatures & M.filter (isA Goblin) & M.keys)
      fmap (fmap (view _2)) rs `shouldBe` [           pure elf, pure elf
                                          , pure elf, Nothing,  pure elf
                                          , pure elf, pure elf, pure elf
                                          ]

  context "with a simple game" $ do
    let Right gs = parseOnly parseInput
                 $ T.unlines ["#######"
                             ,"#.E...#"
                             ,"#.....#"
                             ,"#...G.#"
                             ,"#######"
                             ]

    describe "creaturesToPlay" $ do
      let nextCreature = head . play creaturesToPlay

      it "should select the elf to move" $ do
        let (loc, c) = nextCreature gs

        loc `shouldBe` Coord 1 2
        _alliegance c `shouldBe` Elf

      context "if the positions are reversed" $ do
        let [(a, x), (b, y)] = M.toList (gs^.creatures)
        let gs' = gs { _creatures = M.fromList [(a, y), (b, x)] }

        it "selects the goblin, in the same position" $ do
          let (loc, c) = nextCreature gs'
          loc `shouldBe` Coord 1 2
          _alliegance c `shouldBe` Goblin

    describe "takeTurn" $ do
      it "should move the elf right" $ do
        let (l,c) = M.findMin (gs ^. creatures)
            gs' = runGame (takeTurn l c) gs
            expected = unlines
                      ["After 0 full rounds:"
                      ,"#######"
                      ,"#..E..#"
                      ,"#.....#"
                      ,"#...G.#"
                      ,"#######"
                      ]
        showGameState False gs' `shouldBe` expected

  describe "exampleThree" $ do
    let Right gs = runGame game <$> parseOnly parseInput exampleThree
    it "should end after round 47" $ do
      gs ^. rounds `shouldBe` 47
    it "should have been won by the goblins" $ do
      winner gs `shouldBe` Just (Goblin, 4)
    it "the goblins should have 590 hp in total" $ do
      let hps = sum (gs ^.. currentCreatures.hp)
      hps `shouldBe` 590

  describe "example rounds" $ do
    let solve = fmap (play game) . parseOnly parseInput . T.unlines
        table = [(["#######"
                  ,"#G..#E#"
                  ,"#E#E.E#"
                  ,"#G.##.#"
                  ,"#...#E#"
                  ,"#...E.#"
                  ,"#######" 
                  ], Result Elf 982 37
                 )
                ,(["#######"
                  ,"#E..EG#"
                  ,"#.#G.E#"
                  ,"#E.##E#"
                  ,"#G..#.#"
                  ,"#..E#.#"
                  ,"#######"
                  ], Result Elf 859 46
                 )
                ,(["#######"
                  ,"#.E...#"
                  ,"#.#..G#"
                  ,"#.###.#"
                  ,"#E#G#G#"
                  ,"#...#G#"
                  ,"#######"
                  ], Result Goblin 536 54
                 )
                ,(["#########"
                  ,"#G......#"
                  ,"#.E.#...#"
                  ,"#..##..G#"
                  ,"#...##..#"
                  ,"#...#...#"
                  ,"#.G...G.#"
                  ,"#.....G.#"
                  ,"#########"
                  ], Result Goblin 937 20
                 )
                ]

    forM_ (zip [1 ..] table) $ \(i, (board, expected)) -> do
      it ("should be able to solve board " <> show i) $ do
        solve board `shouldBe` Right expected

  describe "playRound" $ do
    let nAlive s = sum (s ^.. alive.both)
        stepped = do s <- genGameState
                     let s' = runGame playRound s
                     pure (s, s')

    specify "alive never goes up, and is always above zero" $
      withMaxSuccess 20 $
      forAll stepped $ \(before, after) ->
        let survivors = nAlive after
         in 0 <= survivors && survivors <= nAlive before

    specify "remainingElves is a faithful count of the remaining elves" $
      withMaxSuccess 20 $
      forAll stepped $ \(before, after) -> do
        view remainingElves before `shouldBe` count (isA Elf) (before ^.. currentCreatures)
        view remainingElves after  `shouldBe` count (isA Elf) (after ^.. currentCreatures)

    specify "nAlive is the length of currentCreatures" $
      withMaxSuccess 20 $
      forAll stepped $ \(before, after) -> do
        nAlive before `shouldBe` length (before ^.. currentCreatures)
        nAlive after  `shouldBe` length (after ^.. currentCreatures)

    specify "if nAlive goes down, then totalHP goes down" $
      withMaxSuccess 20 $
      forAll genGameState $ \before ->
        let after = applyN 20 (runGame playRound) before
         in (nAlive after < nAlive before) ==> (sumHP after < sumHP before)

    specify "rounds never goes down" $
      withMaxSuccess 20 $
      forAll stepped $ \(before, after) -> do
        view rounds before `shouldSatisfy` (<= view rounds after)
    
    specify "if rounds does not change, then the game is over" $
      withMaxSuccess 10 $
      forAll stepped $ \(before, after) ->
        (view rounds before == view rounds after) ==> play gameOver after

    specify "if there is no movement, then the game is over or damage has been dealt" $
      withMaxSuccess 10 $
      forAll stepped $ \(before, after) ->
        (view moved after == 0) ==> (play gameOver after || sumHP after < sumHP before)

    specify "the game is winnable" $
      withMaxSuccess 20 $
      forAll genGameState $ \s -> do
        let r = play game s 
        fullRounds r `shouldSatisfy` (>= 0)
        totalHP r `shouldSatisfy` (<= sumHP s)

  describe "exampleTwo" $ do
    let Right gs = parseOnly parseInput exampleTwo
        go = playRound

    context "after 1 round" $ do
      let expected = unlines
                     ["After 1 full rounds:"
                     ,"#########"
                     ,"#.G...G.#"
                     ,"#...G...#"
                     ,"#...E..G#"
                     ,"#.G.....#"
                     ,"#.......#"
                     ,"#G..G..G#"
                     ,"#.......#"
                     ,"#########"
                     ]
      it "has moved to the correct position" $ do
        let gs' = runGame go gs
        showGameState False gs' `shouldBe` expected

    context "after 2 rounds" $ do
      let expected = unlines
                     ["After 2 full rounds:"
                     ,"#########"
                     ,"#..G.G..#"
                     ,"#...G...#"
                     ,"#.G.E.G.#"
                     ,"#.......#"
                     ,"#G..G..G#"
                     ,"#.......#"
                     ,"#.......#"
                     ,"#########"
                     ]
      it "has moved to the correct position" $ do
        let gs' = runGame (go >> go) gs
        showGameState False gs' `shouldBe` expected

    context "after 3 rounds" $ do
      let expected = unlines
                     ["After 3 full rounds:"
                     ,"#########"
                     ,"#.......#"
                     ,"#..GGG..#"
                     ,"#..GEG..#"
                     ,"#G..G...#"
                     ,"#......G#"
                     ,"#.......#"
                     ,"#.......#"
                     ,"#########"
                     ]
      it "has moved to the correct position" $ do
        let gs' = runGame (go >> go >> go) gs
        showGameState False gs' `shouldBe` expected

  describe "neighbour" $ do
    specify "all adjacent squares are neighbours" $ property $ \p ->
      all (neighbour p) (nwes p)
    specify "no square is a neighbour of itself" $ property $ \p ->
      not (neighbour p p)
    specify "second order neighbours are not direct neighbours" $ property $ \p ->
      let direct = nwes p
          indirect = (direct >>= nwes) L.\\ direct
       in none (neighbour p) indirect

  describe "attacking" $ do
    let Right gs = parseOnly parseInput $ T.unlines
                   ["#######"
                   ,"#G....#" -- HP: 9
                   ,"#..G..#" -- HP: 4
                   ,"#..EG.#" -- HP: 2
                   ,"#..G..#" -- HP: 2
                   ,"#...G.#" -- HP: 1
                   ,"#######"
                   ]
        goblinHP = M.fromList $ zip [1..] [9,4,2,2,1]
        f loc c = case c ^. alliegance of
                      Elf -> c
                      Goblin -> c & hp .~ (goblinHP M.! row loc)
        gs' = gs & creatures #%~ M.mapWithKey f
        elf = Coord 3 3

    it "should select the goblin on line 3" $ do
      let target = (Coord 3 4, Creature Goblin 2 (CreatureId 3))

      nextTarget Goblin elf (gs'^.creatures) `shouldBe` Just target

    it "should kill the goblin on line 3" $ do
      let s = runGame (attack Goblin (Coord 3 4) elf) gs'
      (s ^. killed & HS.size) `shouldBe` 1
      (s ^. creatures & M.lookup (Coord 3 4)) `shouldBe` Nothing

    it "should kill the goblin on line 3, even if we originally came for the one on line 2" $ do
      let t = Coord 2 3
          s = runGame (attack Goblin t elf) gs'
          goblin state = state ^. creatures & M.lookup t

      (s ^. killed & HS.size) `shouldBe` 1
      goblin s `shouldBe` goblin gs'

  describe "minElfPower" $ do
    let test = fmap (fmap fst . minElfPower) . parseOnly parseInput . T.unlines
        table = [(["#######"
                  ,"#.G...#"
                  ,"#...EG#"
                  ,"#.#.#G#"
                  ,"#..G#E#"
                  ,"#.....#"
                  ,"#######"
                  ], 15
                 )
                ,(["#######"
                  ,"#E..EG#"
                  ,"#.#G.E#"
                  ,"#E.##E#"
                  ,"#G..#.#"
                  ,"#..E#.#"
                  ,"#######"
                  ], 4
                 )
                ,(["#######"
                  ,"#E.G#.#"
                  ,"#.#G..#"
                  ,"#G.#.G#"
                  ,"#G..#.#"
                  ,"#...E.#"
                  ,"#######" 
                  ], 15
                  )
                ,(["#######"
                  ,"#.E...#"
                  ,"#.#..G#"
                  ,"#.###.#"
                  ,"#E#G#G#"
                  ,"#...#G#"
                  ,"#######"
                  ], 12
                 )
                ,(["#########"
                  ,"#G......#"
                  ,"#.E.#...#"
                  ,"#..##..G#"
                  ,"#...##..#"
                  ,"#...#...#"
                  ,"#.G...G.#"
                  ,"#.....G.#"
                  ,"#########"
                  ], 34
                 )
                ]
    forM_ (zip [1 ..] table) $ \(i, (board, expected)) -> do
      let title = unwords ["min elf power required for example"
                          , show i, "is", show expected
                          ]
      specify title $ do
        test board `shouldBe` Right (Just expected)

illustrate :: GameState -> Game a -> IO ()
illustrate s g = let s' = runGame g s
                  in putStrLn (showGameState True s')

-- generate a legal, solveable game state.
-- Legal means:
--  - at least one creature
--  - single, coherent cavern, reachable by all creatures
--  - elfPower between (4..200)
--  - all creatures have HP between (1..200)
--  - between (0..10) rounds have been completed
genGameState :: Gen GameState
genGameState = do
  dng <- genDungeon
  r <- chooseInt (0, 10)
  ep <- chooseInt (attackPower, initialHP)
  creatures <- zip <$> locations dng
                   <*> infiniteListOf creature
                           
  pure $ dungeonState dng creatures
       & rounds .~ r
       & elfPower .~ ep

  where
    locations dng = fmap L.nubOrd . listOf1 $ Test.QuickCheck.elements [ loc | (loc, Open) <- A.assocs dng ]
    creature = do hp <- chooseInt (1, 200)
                  oneof [ mkCreature Elf hp, mkCreature Goblin hp ]


genDungeon :: Gen Dungeon
genDungeon = do
  rows <- Row <$> chooseInt (5, 50)
  cols <- Col <$> chooseInt (5, 50)
  let bs = (Coord.origin, Coord rows cols)
  let squares = A.range bs

  isWall <- flip HS.member . HS.fromList . mconcat <$> listOf (wall squares)

  -- we have a bunch of walls, but we want to ensure that the map is not
  -- partitioned. To do this, we find the largest cavern, and remove all
  -- open squares that cannot reach it, clipping the dungeon bounds if necessary.
  let largestCavern = best length
                    . Clique.cliques
                    . Clique.searchGraph (filter (allPreds [A.inRange bs, not . isWall]) . nwes)
                    . filter (not . isWall)
                    $ squares
  
  pure $ case largestCavern of
    Nothing -> A.listArray bs (repeat Open) -- all walls! useless. Assume all-open
    Just sqs -> let bb = Coord.boundingBox sqs in A.listArray bb (bool Wall Open . Clique.member sqs <$> A.range bb)

  where
    wall locs = do
      loc <- Test.QuickCheck.elements locs
      dirs <- listOf1 (Test.QuickCheck.elements (nwes Coord.origin))
      pure $ scanl Coord.translate loc dirs

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

import Control.Applicative
import GHC.Generics (Generic)
import Data.Hashable
import Data.Functor
import qualified Data.Array as A
import qualified Data.Map.Strict as M
import qualified Data.HashSet as HS
import Control.Lens
import Data.Monoid
import Data.Maybe
import Control.Monad.State.Strict
import qualified Data.List.Extra as L
import           Text.Parser.Char (text)
import qualified Data.Text as T
import           Data.Text (Text)

import           Elves
import           Elves.Advent
import qualified Elves.AStar as AStar
import qualified Elves.StrictGrid as G
import           Elves.Coord.Strict
import qualified Elves.Coord as Coord

type Location = Coordinate
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

data GameState = GameState
  { _dungeon   :: !Dungeon
  , _creatures :: !Creatures
  , _rounds   :: {-# UNPACK #-} !Int
  , _unmoved  :: ![(Location,Creature)]
  , _killed   :: !(HS.HashSet CreatureId)
  , _alive    :: {-# UNPACK #-} !(Int, Int)
  , _elfPower :: {-# UNPACK #-} !Int
  } deriving (Show)

makeLenses ''GameState

aliveLens Goblin = remainingGoblins
aliveLens Elf    = remainingElves

remainingElves, remainingGoblins :: Lens' GameState Int
remainingElves   = alive . _1
remainingGoblins = alive . _2

newtype Game a = Game { runGame :: State GameState a }
  deriving (Functor, Applicative, Monad, MonadState GameState)

main :: IO ()
main = day 15 parseInput pt1 pt2 spec
  where
    pt1 gs = sim gs >>= showOutcome
    pt2 gs = case findMinElfPower gs of
               Nothing -> putStrLn "Unwinnable :("
               Just ep -> do
                 putStrLn $ "MIN ELF POWER: " <> show ep
                 showOutcome . result $ play game gs { _elfPower = ep }

    showOutcome r = putStrLn $ "OUTCOME: " <> show (fullRounds r * totalHP r)


play :: Game a -> GameState -> GameState
play g = execState (runGame g)

currentCreatures :: Traversal' GameState Creature
currentCreatures f gs = 
  let cs = gs ^. creatures & M.toAscList
      fcs = traverse (\(i,c) -> fmap (i,) (f c)) cs
   in fmap (\cs -> gs & creatures .~ M.fromAscList cs) fcs

winner :: GameState -> Maybe Allegiance
winner gs = case gs ^. alive & uncurry compare of
  LT -> Just Goblin
  GT -> Just Elf
  _  -> Nothing

-- all units have the same attack power
attackPower :: Int
attackPower = 3

distance :: GameState -> Location -> Location -> Maybe Int
distance gs origin destination
  = length <$> AStar.aStar neighbours
                (\_ _ -> 1)
                (AStar.euclideanDistance destination)
                (== destination)
                origin
  where
    neighbours = HS.fromList . openSquares (gs & creatures %~ M.delete destination)

nsew :: Location -> [Location]
nsew x = Coord.translate x <$>
             [ Coord (-1) 0
             , Coord 0 (-1)
             , Coord 0 1
             , Coord 1 0
             ]

preLoop :: Game ()
preLoop = do
  cs <- use creatures
  unmoved .= M.toAscList cs

game :: Game ()
game = do
  preLoop
  gameLoop
  finished <- gameOver
  unless finished game

gameOver :: Game Bool
gameOver = uses alive (not . allOf both (> 0))

gameLoop :: Game ()
gameLoop = do
  mc <- nextCreature
  case mc of
    Nothing ->
      rounds #%= succ
    Just (loc, c) -> do 
      noEnemy <- uses (aliveLens (target c)) (== 0)
      if noEnemy then return ()
                 else takeTurn loc c >> gameLoop

nextCreature :: Game (Maybe (Location, Creature))
nextCreature = unmoved %%= pop

pop :: [a] -> (Maybe a, [a])
pop [] = (Nothing, [])
pop (x:xs) = (Just x, xs)

findClosest :: Allegiance -> Location -> Game (Maybe (Int, Location))
findClosest side loc = do
  cs <- use creatures
  s <- get
  return . listToMaybe
         . L.sort
         $ catMaybes [ fmap (,l) md | (l, Creature side' _ cid) <- M.toList cs
                                    , side' == side
                                    , let md = distance s loc l
                                    ]

target :: Creature -> Allegiance
target c = case c ^. alliegance of
             Elf -> Goblin
             Goblin -> Elf

takeTurn :: Location -> Creature -> Game ()
takeTurn pos c = do
  skipped <- uses killed (HS.member (c ^. creatureId))
  unless skipped $ do
    mt <- findClosest (target c) pos
    case mt of
      Nothing -> return ()
      Just (d,t) -> if d >  1
                       then moveTowards c pos t >>= attack (target c)
                       else attack (target c) pos

moveTowards :: Creature -> Location -> Location -> Game Location
moveTowards c current destination = do
  s <- get
  let step = head . sorting s $ openSquares s current
  creatures #%= (M.insert step c . M.delete current)
  return step
  where
    sorting s = fmap snd
              . L.sort
              . filter (isJust . fst)
              . fmap (flip (distance s) destination &&& id)

openSquares :: GameState -> Location -> [Location]
openSquares gs = filter canBeMovedTo . nsew
  where
    canBeMovedTo = getAll . foldMap (All .) [unoccupied, isOpen, inBounds]
    unoccupied = not . flip M.member (gs ^. creatures)
    inBounds = A.inRange (A.bounds (gs ^. dungeon))
    isOpen = (Open ==) . ((gs ^. dungeon) A.!)
  
attack :: Allegiance -> Location -> Game ()
attack side loc = do
  menemy <- uses creatures (nextTarget side loc)
  case menemy of
    Nothing -> return ()
    Just (loc, enemy) -> do
      creatures #%= M.delete loc
      power <- case side of Elf -> pure attackPower
                            Goblin -> use elfPower
      let injured = enemy & hp -~ power
      if dead injured
        then do killed #%= HS.insert (injured ^. creatureId)
                aliveLens (injured ^. alliegance) #%= pred
        else creatures #%= M.insert loc injured

nextTarget :: Allegiance -> Location -> Creatures -> Maybe (Location, Creature)
nextTarget side loc cs
  = listToMaybe
  . L.sortOn attackOrder
  . filter (isA side . snd)
  . mapMaybe (\l -> (l,) <$> M.lookup l cs)
  $ nsew loc

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
      countOf side = length . filter (isA side . snd) $ cs

  let s = dungeonState dng
          & creatures .~ M.fromList cs
          & remainingElves .~ countOf Elf
          & remainingGoblins .~ countOf Goblin

  pure s

dungeonState :: Dungeon -> GameState
dungeonState dng = GameState dng mempty 0 [] mempty (0,0) attackPower

cellP :: Parser (Feature, Maybe Creature)
cellP =   text "#" $> (Wall, Nothing)
      <|> text "." $> (Open, Nothing)
      <|> text "E" $> (Open, mkCreature Elf)
      <|> text "G" $> (Open, mkCreature Goblin)
  where
    mkCreature a = pure $ Creature a 200 (CreatureId 0)

showGameState :: Bool -> GameState -> String
showGameState withCs gs
  = unlines [ mconcat ["After ", show (gs ^. rounds), " rounds:"]
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

data Result = Result { wonBy :: Maybe Allegiance
                     , totalHP :: Int
                     , fullRounds :: Int
                     }
  deriving (Show, Eq)

result :: GameState -> Result
result gs = Result (winner gs)
                   (sum (gs ^.. currentCreatures.hp))
                   (gs ^. rounds)

sim :: GameState -> IO Result
sim gs = do
  let gs' = play (preLoop >> gameLoop) gs
  putStrLn (showGameState True gs')
  if evalState (runGame gameOver) gs'
     then return (result gs')
     else sim gs'

-- binary search to find a the lowest possible power
findMinElfPower :: GameState -> Maybe Int
findMinElfPower gs = go (4,200)
  where 
    e0 = gs ^. remainingElves
    go rng = case A.rangeSize rng of
      0 -> Nothing
      1 -> Just (fst rng)
      _ -> let p = uncurry Coord.midpoint rng
               gs' = play game gs { _elfPower = p }
            in if e0 == gs' ^. remainingElves
               then go (fst rng, p)
               else go (p + 1, snd rng)

spec :: Spec
spec = do
  describe "moveTowards" $ do
    let Right gs = parseOnly parseInput
                 $ T.unlines ["#######"
                             ,"#.E...#"
                             ,"#.....#"
                             ,"#...G.#"
                             ,"#######"
                             ]
    it "should select the elf to move" $ do
      let mc = fst <$> evalState (runGame (preLoop >> nextCreature)) gs
      mc `shouldBe` Just (Coord 1 2)
    it "should move the elf right" $ do
      let (l,c) = gs ^. creatures & M.findMin
          gs' = play (takeTurn l c) gs
          expected = unlines
                    ["After 0 rounds:"
                    ,"#######"
                    ,"#..E..#"
                    ,"#.....#"
                    ,"#...G.#"
                    ,"#######"
                    ]
      showGameState False gs' `shouldBe` expected

  describe "exampleThree" $ do
    let Right gs = play game <$> parseOnly parseInput exampleThree
    it "should end after round 47" $ do
      gs ^. rounds `shouldBe` 47
    it "should have been won by the goblins" $ do
      winner gs `shouldBe` Just Goblin
    it "the goblins should have 590 hp in total" $ do
      let hps = sum (gs ^.. currentCreatures.hp)
      hps `shouldBe` 590

  describe "example rounds" $ do
    let solve = fmap (result . play game) . parseOnly parseInput . T.unlines
        table = [(["#######"
                  ,"#G..#E#"
                  ,"#E#E.E#"
                  ,"#G.##.#"
                  ,"#...#E#"
                  ,"#...E.#"
                  ,"#######" 
                  ], Result (Just Elf) 982 37
                 )
                ,(["#######"
                  ,"#E..EG#"
                  ,"#.#G.E#"
                  ,"#E.##E#"
                  ,"#G..#.#"
                  ,"#..E#.#"
                  ,"#######"
                  ], Result (Just Elf) 859 46
                 )
                ,(["#######"
                  ,"#.E...#"
                  ,"#.#..G#"
                  ,"#.###.#"
                  ,"#E#G#G#"
                  ,"#...#G#"
                  ,"#######"
                  ], Result (Just Goblin) 536 54
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
                  ], Result (Just Goblin) 937 20
                 )
                ]

    forM_ (zip [1 ..] table) $ \(i, (board, expected)) -> do
      it ("should be able to solve board " <> show i) $ do
        solve board `shouldBe` Right expected

  describe "exampleTwo" $ do
    let Right gs = parseOnly parseInput exampleTwo
        go = preLoop >> gameLoop
        expected = unlines
                   ["After 3 rounds:"
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
    it "has moved to the correct position after 3 rounds" $ do
      let gs' = play (go >> go >> go) gs
      showGameState False gs' `shouldBe` expected

  describe "attacking" $ do
    let Right gs = parseOnly parseInput $ T.unlines
                   ["#######"
                   ,"#G....#"
                   ,"#..G..#"
                   ,"#..EG.#"
                   ,"#..G..#"
                   ,"#...G.#"
                   ,"#######"
                   ]
        goblinHP = M.fromList $ zip [1..] [9,4,2,2,1]
        f loc c = case c ^. alliegance of
                      Elf -> c
                      Goblin -> c & hp .~ (goblinHP M.! row loc)
        gs' = gs & creatures #%~ M.mapWithKey f

    it "should select the goblin on line 3" $ do
      let target = (Coord 3 4, Creature Goblin 2 (CreatureId 3))

      nextTarget Goblin (Coord 3 3) (gs'^.creatures) `shouldBe` Just target

    it "should kill the goblin on line 3" $ do
      let s = execState (runGame $ attack Goblin (Coord 3 3)) gs'
      (s ^. killed & HS.size) `shouldBe` 1

  describe "findMinElfPower" $ do
    let test = fmap findMinElfPower . parseOnly parseInput . T.unlines
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


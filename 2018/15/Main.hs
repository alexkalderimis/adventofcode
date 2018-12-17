{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE DeriveGeneric #-}

import Control.Applicative
import Test.Hspec
import GHC.Generics (Generic)
import Data.Hashable
import Control.Arrow
import qualified Data.Array as A
import qualified Data.Map.Strict as M
import qualified Data.HashSet as HS
import Control.Monad
import Data.Ord
import Data.Bool
import Control.Lens
import Data.Monoid
import Data.Maybe
import Control.Monad.State.Strict
import qualified Data.List as L
import qualified Elves.AStar as AStar

type Location = (Int,Int)
type Dungeon = A.Array Location Feature
type Creatures = M.Map Location Creature

data Direction = N | S | E | W deriving (Show, Eq)
data Feature = Wall | Open deriving (Show, Eq)

data Allegiance = Elf | Goblin deriving (Show, Eq, Generic)

instance Hashable Allegiance

newtype CreatureId = CreatureId Int deriving (Show, Eq, Hashable)

data Creature = Creature
  { _alliegance :: Allegiance
  , _hp :: Int
  , _creatureId :: CreatureId
  } deriving (Show, Eq)

makeLenses ''Creature

data GameState = GameState
  { _dungeon :: Dungeon
  , _creatures :: Creatures
  , _rounds :: Int
  , _unmoved :: [(Location,Creature)]
  , _killed :: HS.HashSet CreatureId
  , _alive :: (Int, Int)
  } deriving (Show)

makeLenses ''GameState

newtype Game a = Game { runGame :: State GameState a }
  deriving (Functor, Applicative, Monad, MonadState GameState)

play :: Game a -> GameState -> GameState
play g s = execState (runGame g) s

currentCreatures :: Traversal' GameState Creature
currentCreatures f gs = 
  let cs = gs ^. creatures & M.toAscList
      fcs = traverse (\(i,c) -> fmap (i,) (f c)) cs
   in fmap (\cs -> gs & creatures .~ (M.fromAscList cs)) fcs

winner :: GameState -> Maybe Allegiance
winner gs = case gs ^. alive & uncurry compare of
  LT -> Just Goblin
  GT -> Just Elf
  _  -> Nothing

-- all units have the same attack power
attackPower :: Int
attackPower = 3

mkCreature :: CreatureId -> Allegiance -> Creature
mkCreature cid a = Creature a 200 cid

distance :: GameState -> Location -> Location -> Maybe Int
distance gs origin destination
  = fmap length
  $ AStar.aStar neighbours
                (\_ _ -> 1)
                (AStar.euclideanDistance destination)
                (== destination)
                origin
  where
    neighbours = HS.fromList . openSquares (gs & creatures %~ M.delete destination)

nsew :: Location -> [Location]
nsew (y,x) = [         (y-1,x)
             ,(y,x -1),       (y,x+1)
                      ,(y+1,x)
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
  if finished then return () else game

gameOver :: Game Bool
gameOver = uses alive (not . allOf both (> 0))

gameLoop :: Game ()
gameLoop = do
  mc <- nextCreature
  case mc of
    Nothing ->
      rounds #%= succ
    Just (loc, c) -> do 
      noEnemy <- uses (alive . aliveLens (target c)) (== 0)
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
  when (not skipped) $ do
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
  menemy <- selectTarget side loc
  case menemy of
    Nothing -> return ()
    Just (loc, enemy) -> do
      creatures #%= M.delete loc
      let injured = enemy & hp -~ attackPower
      if dead injured
        then do killed #%= HS.insert (injured ^. creatureId)
                alive  #%= (aliveLens (injured ^. alliegance) -~ 1)
        else creatures #%= (M.insert loc injured)

selectTarget :: Allegiance -> Location -> Game (Maybe (Location, Creature))
selectTarget side loc = do
  cs <- use creatures
  return $ listToMaybe
         . L.sortBy (comparing (view hp . snd &&& fst))
         . filter ((== side) . view alliegance . snd)
         . catMaybes
         . fmap (\l -> (l,) <$> M.lookup l cs)
         $ nsew loc

aliveLens Goblin = _2
aliveLens Elf = _1

dead :: Creature -> Bool
dead = (<= 0) . view hp

parseInput :: String -> GameState
parseInput str =
  let strs = lines str
      cells = [((y,x), c) | (y, row) <- zip [0 ..] strs
                         , (x, c) <- zip [0 ..] row
              ]
      tiles = zipWith (\i (loc, c) -> cellP (CreatureId i) loc c) [0..] cells
      walls = HS.fromList [loc | (loc, Wall, _) <- tiles]
      cs = M.fromList [(loc, crt) | (loc, _, Just crt) <- tiles]
      bounds = ((0,0), (length strs - 1, maximum (fmap length strs) - 1))
      dng = A.array bounds [(loc, bool Open Wall (HS.member loc walls))
                                                   | loc <- A.range bounds]
   in GameState dng cs 0 [] mempty (length (filter (== 'E') str)
                                   ,length (filter (== 'G') str)
                                   )

cellP :: CreatureId -> Location -> Char -> (Location, Feature, Maybe Creature)
cellP cid loc c = uncurry (loc,,) $ case c of
  '#' -> (Wall, Nothing)
  '.' -> (Open, Nothing)
  'E' -> (Open, Just $ mkCreature cid Elf)
  'G' -> (Open, Just $ mkCreature cid Goblin)
  _   -> error $ "Cannot parse dungeon tile: " ++ [c]

showGameState :: Bool -> GameState -> String
showGameState withCs gs
  = unlines $ ("After " ++ show (gs ^. rounds) ++ " rounds:")
            : fmap showRow [r0 .. rN]
  where
    ((r0,c0),(rN,cN)) = A.bounds (gs ^. dungeon)
    showCreature c = concat [showSide (c ^. alliegance) 
                            ,"("
                            ,show (c ^. hp)
                            ,")"
                            ]
    showSide Elf = "E"
    showSide Goblin = "G"
    showRow y = fmap (showTile y) [c0 .. cN] ++ concat [csSummary y | withCs]
    csSummary y = ("  " ++ )
                . L.intercalate ", "
                . fmap (showCreature . snd)
                . filter ((y ==) . fst . fst)
                $ (gs ^. creatures & M.toAscList)

    showTile y x = let loc = (y,x)
                    in tileFor ((gs ^. dungeon) A.! loc)
                               (gs ^. creatures & M.lookup loc & fmap (view alliegance))
    tileFor Wall _ = '#'
    tileFor _ (Just Elf) = 'E'
    tileFor _ (Just Goblin) = 'G'
    tileFor _ _ = '.'

exampleOne :: String
exampleOne = unlines
  ["#######"
  ,"#E..G.#"
  ,"#...#.#"
  ,"#.G.#G#"
  ,"#######"
  ]
                      
exampleTwo :: String
exampleTwo = unlines
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

exampleThree :: String
exampleThree = unlines
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
                     , fullRounds :: Int }
  deriving (Show, Eq)

result :: GameState -> Result
result gs = Result (winner gs) (sum (gs ^.. currentCreatures.hp)) (gs ^. rounds)

sim :: GameState -> IO ()
sim gs = do
  let gs' = play (preLoop >> gameLoop) gs
  putStrLn (showGameState True gs')
  if evalState (runGame gameOver) gs'
     then return ()
     else sim gs'

spec :: Spec
spec = do
  describe "moveTowards" $ do
    let gs = parseInput $ unlines ["#######"
                                  ,"#.E...#"
                                  ,"#.....#"
                                  ,"#...G.#"
                                  ,"#######"
                                  ]
    it "should select the elf to move" $ do
      let mc = fst <$> evalState (runGame (preLoop >> nextCreature)) gs
      mc `shouldBe` Just (1,2)
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
    let gs = play game (parseInput exampleThree)
    it "should end after round 47" $ do
      gs ^. rounds `shouldBe` 47
    it "should have been won by the goblins" $ do
      winner gs `shouldBe` Just Goblin
    it "the goblins should have 590 hp in total" $ do
      let hps = sum (gs ^.. currentCreatures.hp)
      hps `shouldBe` 590
  describe "example rounds" $ do
    let go = result . play game . parseInput
        table = [(unlines
                  ["#######"
                  ,"#G..#E#"
                  ,"#E#E.E#"
                  ,"#G.##.#"
                  ,"#...#E#"
                  ,"#...E.#"
                  ,"#######"
                  ], Result (Just Elf) 982 37
                 )
                ,(unlines
                  ["#######"
                  ,"#E..EG#"
                  ,"#.#G.E#"
                  ,"#E.##E#"
                  ,"#G..#.#"
                  ,"#..E#.#"
                  ,"#######"
                  ], Result (Just Elf) 859 46
                 )
                ,(unlines
                  ["#######"
                  ,"#.E...#"
                  ,"#.#..G#"
                  ,"#.###.#"
                  ,"#E#G#G#"
                  ,"#...#G#"
                  ,"#######"
                  ], Result (Just Goblin) 536 54
                 )
                ,(unlines
                  ["#########"
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
      it ("should be able to solve board " ++ show i) $ do
        go board `shouldBe` expected

  describe "exampleTwo" $ do
    let gs = parseInput exampleTwo
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
    let gs = parseInput $ unlines ["#######"
                                  ,"#G....#"
                                  ,"#..G..#"
                                  ,"#..EG.#"
                                  ,"#..G..#"
                                  ,"#...G.#"
                                  ,"#######"
                                  ]
        goblinHP = M.fromList $ zip [1..] [9,4,2,2,1]
        f (y,x) c = case c ^. alliegance of
                      Elf -> c
                      Goblin -> c & hp .~ (goblinHP M.! y)
        gs' = gs & creatures #%~ M.mapWithKey f
    it "should select the goblin on line 3" $ do
      fmap fst (evalState (runGame $ selectTarget Goblin (3,3)) gs')
        `shouldBe` Just (3,4)
    it "should kill the goblin on line 3" $ do
      let s = execState (runGame $ attack Goblin (3,3)) gs'
      (s ^. killed & HS.size) `shouldBe` 1


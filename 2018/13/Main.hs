{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

import           Data.Functor
import qualified Data.Array as A
import           Data.Foldable
import qualified Data.Text as T
import           Data.Text (Text)
import           Data.Maybe
import qualified Data.Set           as S
import qualified Data.Map.Strict    as M
import           Data.Traversable   (for)
import           Text.Printf
import qualified Data.Attoparsec.Text as Atto
import           Text.Parser.Char (newline, text)

import Elves
import Elves.Advent
import Elves.Coord.Strict
import qualified Elves.StrictGrid as G
import Elves.Coord (translate)

data Turn = L | Straight | R deriving (Show, Eq, Enum, Bounded)

data Direction = N | S | E | W
  deriving (Show, Eq)

data TrackSegment = NS | EW | Intersection | FwdSlash | BackSlash | NoTrack
  deriving (Show, Eq, Enum)

data Policy = AllowCollisions | RemoveCarts deriving (Show, Eq)

data Cart = Cart
  { cartDirection :: !Direction
  , cartLastTurn  :: !Turn
  } deriving (Show, Eq)

type NetworkMap = A.Array Coordinate TrackSegment

type Carts = [(Coordinate, Cart)]

main :: IO ()
main = day 13 parseInput pt1 pt2 spec
  where
    pt1 = run AllowCollisions
    pt2 = run RemoveCarts
    run :: Policy -> (NetworkMap, Carts) -> IO ()
    run policy (nw, carts) = do
        printf "%d carts\n" (length carts)
        let go = \mcs _ -> mcs >>= tick policy nw
            (Coord y x) = head [loc | Left loc <- scanl go (pure carts) (repeat ())]
            msg = case policy of AllowCollisions -> "Collision"
                                 RemoveCarts     -> "Last Cart"
        printf "%s: %d,%d\n" (msg :: String) x y

runN :: Int -> Policy -> NetworkMap -> Carts -> Maybe Coordinate
runN 0 _ _ _ = Nothing
runN n p nw cs = case tick p nw cs of
                   Left loc -> pure loc
                   Right cs' -> runN (pred n) p nw cs'

parseInput :: Parser (NetworkMap, Carts)
parseInput = do
  cells <- G.gridP cellP
  let network = fmap fst cells
      carts = mapMaybe (\(loc, mc) -> (loc,) <$> mc) . A.assocs $ fmap snd cells
  pure (network, carts)

cellP :: Parser (TrackSegment, Maybe Cart)
cellP =   text "/"  $> (FwdSlash, Nothing)
      <|> text "\\" $> (BackSlash, Nothing)
      <|> text "-"  $> (EW, Nothing)
      <|> text "|"  $> (NS, Nothing)
      <|> text "+"  $> (Intersection, Nothing)
      <|> text ">"  $> (EW, mkCart E)
      <|> text "<"  $> (EW, mkCart W)
      <|> text "^"  $> (NS, mkCart N)
      <|> text "v"  $> (NS, mkCart S)
      <|> text " "  $> (NoTrack, Nothing)
  where
      mkCart d = pure $ Cart d maxBound

-- requires that the location be in bounds. This is a good
-- thing, since attempting to get a segment off the map suggests
-- we have gone off-track, in the literal sense.
trackSegment :: NetworkMap -> Coordinate -> TrackSegment
trackSegment = (A.!)

-- shows state at each tick. Useful in the repl
runSim :: Policy -> Int -> NetworkMap -> Carts -> IO ()
runSim _ 0 _ _ = putStrLn "Limit reached!"
runSim p limit nmap carts = do
  putStrLn (showState nmap carts)
  case tick p nmap carts of
    Left loc -> printf "Crash! (%d, %d)" (row loc) (col loc)
    Right cs -> putStrLn (replicate 20 '-')
                >> runSim p (pred limit) nmap cs

-- on each tick move the carts about.
-- If there is a collision, then we 'error' with that location.
tick :: Policy -> NetworkMap -> Carts -> Either Coordinate Carts
tick policy nmap carts = case carts of
  []        -> error "No carts"
  [(loc,_)] -> Left loc
  _         -> M.toList . fst <$> foldM microtick (M.fromList carts, S.empty) carts
  where
    -- semantically richer helpers
    skip s loc            = S.member loc (snd s)
    occupied s loc        = M.member loc (fst s)
    moveTo s old new c    = (M.delete old $ M.insert new c (fst s), snd s)
    removeColliding (m, s) a b = (foldr M.delete m [a, b], S.insert b s)
    -- we need to know both the positions of other carts (so the carts
    -- map) as well as carts that have been removed on this tick, so that
    -- they can be skipped over when they turn up.
    microtick s (loc, _) | skip s loc = pure s
    microtick s (loc, c) = do
      let c'   = turnCart (trackSegment nmap loc) c
          loc' = continueAhead c' loc
      case (occupied s loc', policy) of
        (False, _)           -> pure (moveTo s loc loc' c')
        (_, RemoveCarts)     -> pure (removeColliding s loc loc')
        (_, AllowCollisions) -> Left loc'

turnCart :: TrackSegment -> Cart -> Cart
turnCart seg c = case seg of
  NoTrack -> error "Ran out of track!"
  Intersection -> let t = nextTurn (cartLastTurn c)
                      d = applyTurn t (cartDirection c)
                   in Cart d t
  _ -> let t = followTrack seg (cartDirection c)
           d = applyTurn t (cartDirection c)
        in c { cartDirection = d }

followTrack :: TrackSegment -> Direction -> Turn
followTrack FwdSlash N  = R
followTrack FwdSlash S  = R
followTrack FwdSlash E  = L
followTrack FwdSlash W  = L
followTrack BackSlash N = L
followTrack BackSlash S = L
followTrack BackSlash E = R
followTrack BackSlash W = R
followTrack _ _         = Straight

continueAhead :: Cart -> Coordinate -> Coordinate
continueAhead c = moveInDir (cartDirection c)

moveInDir :: Direction -> Coordinate -> Coordinate
moveInDir N = translate $ Coord (-1)  0
moveInDir S = translate $ Coord   1   0
moveInDir E = translate $ Coord   0   1
moveInDir W = translate $ Coord   0 (-1)

nextTurn :: Turn -> Turn
nextTurn = cycleSucc

applyTurn :: Turn -> Direction -> Direction
applyTurn Straight d = d
applyTurn L N        = W
applyTurn L S        = E
applyTurn L E        = N
applyTurn L W        = S
applyTurn R N        = E
applyTurn R S        = W
applyTurn R E        = S
applyTurn R W        = N

showState :: NetworkMap -> Carts -> String
showState nmap carts = G.draw (fmap drawCell nmap A.// cartOverlay)
  where
    cartOverlay = second (drawCart . cartDirection) <$> carts

    drawCell NS           = '|'
    drawCell EW           = '-'
    drawCell Intersection = '+'
    drawCell FwdSlash     = '/'
    drawCell BackSlash    = '\\'
    drawCell NoTrack      = ' '

    drawCart N = '^'
    drawCart S = 'v'
    drawCart W = '<'
    drawCart E = '>'

exampleTrack :: Text
exampleTrack = entracken
  ["╭->-╮        "
  ,"|   |  ╭----╮"
  ,"| ╭-+--+-╮  |"
  ,"| | |  | v  |"
  ,"╰-+-╯  ╰-+--╯"
  ,"  ╰------╯   "
  ]

removalTrack :: Text
removalTrack = entracken
  ["╭>-<╮  "
  ,"|   |  "
  ,"| ╭<+-╮"
  ,"| | | v"
  ,"╰>+<╯ |"
  ,"  |   ^"
  ,"  ╰<->╯"
  ]

infiniteTrack :: Text
infiniteTrack = entracken
  ["╭->-╮ "
  ,"|   | "
  ,"|   | "
  ,"|   | "
  ,"╰-<-╯ "
  ]

straightLine :: Text
straightLine = entracken ["->---<-"]

-- avoid using slashes, for the pretty
entracken :: [Text] -> Text
entracken = T.replace "╭" "/"
          . T.replace "╯" "/"
          . T.replace "╮" "\\"
          . T.replace "╰" "\\"
          . T.unlines

spec :: Spec
spec = do
  describe "removalTrack" $ do
    let Right (nw,cs) = parseOnly parseInput removalTrack
    it "should find the last cart location" $ do
      runN 100 RemoveCarts nw cs `shouldBe` Just (Coord 4 6)
  describe "exampleTrack" $ do
    let Right (nw,cs) = parseOnly parseInput exampleTrack
    it "should find the last cart location" $ do
      runN 100 AllowCollisions nw cs `shouldBe` Just (Coord 3 7)
  describe "infiniteTrack" $ do
    let Right (nw,cs) = parseOnly parseInput infiniteTrack
    it "should find the last cart location" $ do
      runN 1000 AllowCollisions nw cs `shouldBe` Nothing


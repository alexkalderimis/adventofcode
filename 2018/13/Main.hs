import           Control.Monad
import           Control.Monad.ST
import           Data.Array.ST      as STA
import qualified Data.Array.Unboxed as A
import           Data.Foldable
import           Data.Int
import qualified Data.Map.Strict    as M
import           Data.Maybe
import qualified Data.Set           as S
import           Data.Traversable   (for)
import           System.Environment
import           System.Exit
import           Test.Hspec
import           Test.QuickCheck    (Arbitrary, arbitrary, property)
import           Text.Printf

data Turn = L | Straight | R deriving (Show, Eq, Enum, Bounded)

data Direction = N | S | E | W
  deriving (Show, Eq)

data TrackSegment = NS | EW | Intersection | FwdSlash | BackSlash
                  | NoTrack
  deriving (Show, Eq, Enum)

type Location = (Int, Int)

data Policy = AllowCollisions | RemoveCarts deriving (Show, Eq)

data Cart = Cart
  { cartDirection :: !Direction
  , cartLastTurn  :: !Turn
  } deriving (Show, Eq)

-- store network map as unboxed array of bytes. Wrap
-- access to track segments via Enum, going through Int
type NetworkMap = A.UArray Location Int8

type Carts = M.Map Location Cart

main :: IO ()
main = do
  (nw, carts) <- parseInput <$> getContents
  args <- getArgs
  policy <- case args of [] -> pure AllowCollisions
                         ["pt2"] -> pure RemoveCarts
                         _ -> die $ "Unexpected arguments " ++ show args

  printf "%d carts\n" (M.size carts)
  let go = \mcs _ -> mcs >>= tick policy nw
      (y,x) = head [loc | Left loc <- scanl go (pure carts) (repeat ())]
      msg = case policy of AllowCollisions -> "Collision"
                           RemoveCarts     -> "Last Cart"
  printf "%s: %d,%d\n" (msg :: String) x y

translate :: Location -> Location -> Location
translate (dy,dx) (y,x) = (y + dy, x + dx)

runN :: Int -> Policy -> NetworkMap -> Carts -> Maybe Location
runN 0 _ _ _ = Nothing
runN n p nw cs = case tick p nw cs of
                   Left loc -> pure loc
                   Right cs' -> runN (pred n) p nw cs'

parseInput :: String -> (NetworkMap, Carts)
parseInput str =
  let strs = lines str
      cells = catMaybes [cellP (y,x) c | (y, row) <- zip [0 ..] strs
                                     , (x, c) <- zip [0 ..] row
                        ]
      nmap = M.fromList [(loc, seg) | (loc, seg, _) <- cells]
      cmap = M.fromList [(loc, crt) | (loc, _, Just crt) <- cells]
      bounds = ((0,0), (length strs - 1, maximum (fmap length strs) - 1))
      network = A.array bounds [(loc, encodeSeg (M.lookup loc nmap)) | loc <- A.range bounds]
   in (network, cmap)
  where
    encodeSeg mseg = toEnum . fromEnum $ fromMaybe NoTrack mseg

cellP :: Location -> Char -> Maybe (Location, TrackSegment, Maybe Cart)
cellP loc c = case c of
  '/'  -> Just (loc, FwdSlash, Nothing)
  '\\' -> Just (loc, BackSlash, Nothing)
  '-'  -> Just (loc, EW, Nothing)
  '|'  -> Just (loc, NS, Nothing)
  '+'  -> Just (loc, Intersection, Nothing)
  '>'  -> Just (loc, EW, mkCart E)
  '<'  -> Just (loc, EW, mkCart W)
  '^'  -> Just (loc, NS, mkCart N)
  'v'  -> Just (loc, NS, mkCart S)
  _    -> Nothing
  where
      mkCart d = pure $ Cart d maxBound

-- requires that the location be in bounds. This is a good
-- thing, since attempting to get a segment off the map suggests
-- we have gone off-track, in the literal sense.
trackSegment :: NetworkMap -> Location -> TrackSegment
trackSegment map loc = toEnum . fromEnum $ map A.! loc

-- shows state at each tick. Usefull in the repl
runSim :: Policy -> Int -> NetworkMap -> Carts -> IO ()
runSim _ 0 _ _ = putStrLn "Limit reached!"
runSim p limit nmap carts = do
  putStrLn (showState nmap carts)
  case tick p nmap carts of
    Left loc -> putStrLn $ "Crash! " ++ show loc
    Right cs -> putStrLn (replicate 20 '-')
                >> runSim p (pred limit) nmap cs

-- on each tick move the carts about.
-- If there a collision, then we 'error' with that location.
tick :: Policy -> NetworkMap -> Carts -> Either Location Carts
tick policy nmap carts = case M.toAscList carts of
  []        -> error "No carts"
  [(loc,_)] -> Left loc
  positions -> fmap fst $ foldM microtick (carts,mempty) positions
  where
    -- semantically richer helpers
    isRemoved s loc       = S.member loc (snd s)
    occupant s loc        = M.lookup loc (fst s)
    moveTo s old new c    = (M.delete old $ M.insert new c (fst s), snd s)
    removeColliding s a b = (foldr M.delete (fst s) [a, b], S.insert b (snd s))
    -- we need to know both the positions of other carts (so the carts
    -- map) as well as carts that have been removed on this tick, so that
    -- they can be skipped over when they turn up.
    microtick s (loc, _) | isRemoved s loc = pure s
    microtick s (loc, c) = do
      let c'   = turnCart (trackSegment nmap loc) c
          loc' = continueAhead c' loc
      case (occupant s loc', policy) of
        (Nothing, _)         -> return (moveTo s loc loc' c')
        (_, RemoveCarts)     -> return (removeColliding s loc loc')
        (_, AllowCollisions) -> Left loc'

turnCart :: TrackSegment -> Cart -> Cart
turnCart seg c = case seg of
  Intersection -> let t = nextTurn (cartLastTurn c)
                      d = applyTurn t (cartDirection c)
                   in Cart d t
  _ -> let t = forcedTurn seg (cartDirection c)
           d = applyTurn t (cartDirection c)
        in c { cartDirection = d }

forcedTurn :: TrackSegment -> Direction -> Turn
forcedTurn FwdSlash N  = R
forcedTurn FwdSlash S  = R
forcedTurn FwdSlash E  = L
forcedTurn FwdSlash W  = L
forcedTurn BackSlash N = L
forcedTurn BackSlash S = L
forcedTurn BackSlash E = R
forcedTurn BackSlash W = R
forcedTurn _ _         = Straight

continueAhead :: Cart -> Location -> Location
continueAhead c = moveInDir (cartDirection c)

moveInDir :: Direction -> Location -> Location
moveInDir N = translate (-1, 0)
moveInDir S = translate ( 1, 0)
moveInDir E = translate (0,  1)
moveInDir W = translate (0, -1)

nextTurn :: Turn -> Turn
nextTurn t | t == maxBound = minBound
nextTurn t = succ t

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
showState nmap carts =
  let ((y0,x0), (yN,xN)) = A.bounds nmap
      row y = flip fmap [x0 .. xN] $ \x ->
                let loc = (y,x)
                 in case (trackSegment nmap loc, M.lookup loc carts) of
                      (_, Just c) -> case cartDirection c of
                                       N -> '^'
                                       S -> 'v'
                                       W -> '<'
                                       E -> '>'
                      (seg, _) -> case toEnum . fromEnum $ seg of
                                    NS           -> '|'
                                    EW           -> '-'
                                    Intersection -> '+'
                                    FwdSlash     -> '/'
                                    BackSlash    -> '\\'
                                    NoTrack      -> ' '
   in unlines (fmap row [y0 .. yN])

exampleTrack :: String
exampleTrack = unlines
  ["/->-\\        "
  ,"|   |  /----\\"
  ,"| /-+--+-\\  |"
  ,"| | |  | v  |"
  ,"\\-+-/  \\-+--/"
  ,"  \\------/   "
  ]

removalTrack :: String
removalTrack = unlines
  ["/>-<\\"
  ,"|   |  "
  ,"| /<+-\\"
  ,"| | | v"
  ,"\\>+</ |"
  ,"  |   ^"
  ,"  \\<->/"
  ]

infiniteTrack :: String
infiniteTrack = unlines
  ["/->-\\"
  ,"|   | "
  ,"|   | "
  ,"|   | "
  ,"\\-<-/ "
  ]

straightLine :: String
straightLine = "->---<-"

spec :: Spec
spec = do
  describe "removalTrack" $ do
    let (nw,cs) = parseInput removalTrack
    it "should find the last cart location" $ do
      runN 100 RemoveCarts nw cs `shouldBe` Just (4,6)
  describe "exampleTrack" $ do
    let (nw,cs) = parseInput exampleTrack
    it "should find the last cart location" $ do
      runN 100 AllowCollisions nw cs `shouldBe` Just (3,7)
  describe "infiniteTrack" $ do
    let (nw,cs) = parseInput infiniteTrack
    it "should find the last cart location" $ do
      runN 1000 AllowCollisions nw cs `shouldBe` Nothing


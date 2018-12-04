import Data.Ord (comparing)
import Data.Maybe
import qualified Data.List as L
import qualified Data.Map.Strict as M

-- Part 1:
-- Solved by establishing 1st the rank of the address, (the ring of the spiral) and then
-- working out the manhattan distance by getting first to a midpoint on the ring
-- and then to the origin. Co-ordinates are not used.
type Rank = Int
type Addr = Int

main :: IO ()
main = do
  input <- read <$> getLine 
  putStrLn $ "Distance from addr: " ++ show (manhattan input)
  putStrLn $ "First stress-test value greater than input: " ++ show (stressTest input)

size :: Rank -> Int
size 0 = 1
size i = 2 * side + 2 * (side - 2)
  where side = 2 * i + 1

minStep :: Rank -> Int
minStep = id

maxStep :: Rank -> Int
maxStep = (2 *)

pageStart :: Rank -> Addr
pageStart r = 1 + sum [size i | i <- [0 .. pred r]]

sideLength :: Rank -> Int
sideLength = maxStep

corners :: Rank -> (Addr, Addr, Addr, Addr)
corners 0 = (1,1,1,1)
corners i = let a = pageStart i - 1
                d = sideLength i
             in (a + d, a + 2 * d, a + 3 * d, a + 4 * d)

mids :: Rank -> (Addr, Addr, Addr, Addr)
mids 0 = (1,1,1,1)
mids i = let (a,b,c,d) = corners i
          in (a - i, b - i, c - i, d - i)

rank :: Addr -> Rank
rank addr = go 0 0
  where
    addr' = addr - 1
    go i rankStart | addr' < rankStart + size i = i
    go i rankStart = go (i + 1) (rankStart + size i)

-- this is the faster implementation, but the one below is simpler
manhattan :: Addr -> Int
manhattan addr = let r = rank addr
                     (a,b,c,d) = mids r
                     closest = L.minimumBy (comparing (straightLine addr)) [a,b,c,d]
                  in straightLine closest addr + minStep r

----------------------------
  -- Part 2: We solve this by building an infinite list of the coordinates
  -- in initialisation order, using the spiral directions to generate them
  -- stepwise. As x-y coordinates, this makes manhattan distances simple, but
  -- it is slower to solve for high indices. This list of coordinates can
  -- then be used to generate values based on the coordinates, or as in the
  -- case of the stress test, as a function of the address and the current
  -- memory state.
  --
  -- Sequence based system
  -- 17  16  15  14  13
  -- 18   5   4   3  12
  -- 19   6   1   2  11
  -- 20   7   8   9  10
  -- 21  22  23---> ...
  --
  -- unrolled:
  -- 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27
  --  R U L L D D R R R  U  U  U  L  L  L  L  D  D  D  D  R  R  R  R  R  R  ..
  --
  -- or:
  --  ----`
  --  1xR |
  --  1xU | <- one cycle
  --  2xL |
  --  2xD /
  --  --
  --  3xR
  --  3xU
  --  4xL
  --  4xD
  --  --
  --  5xR
  --  5xU
  --  ...
 
type Coord = (Int, Int)
type Memory a = M.Map Coord a

data Direction = L | R | U | D
  deriving (Show)

origin :: Coord
origin = (0,0)

straightLine :: Int -> Int -> Int
straightLine a b = abs (a - b)

-- manhattan distance between coordinates
distance :: Coord -> Coord -> Int
distance (x0,y0) (x1,y1) = straightLine x0 x1 + straightLine y0 y1

-- the eight neighbours of any coordinate
neighbours :: Coord -> [Coord]
neighbours (x,y) = [(x - 1, y + 1), (x, y + 1), (x + 1, y + 1)
                   ,(x - 1, y    ),             (x + 1, y    )
                   ,(x - 1, y - 1), (x, y - 1), (x + 1, y - 1)
                   ]

-- infinite list of coordinates starting at the origin and spiralling outwards
coordinates :: [Coord]
coordinates = scanl move origin moves
  where
    move (x,y) U = (x, y + 1)
    move (x,y) D = (x, y - 1)
    move (x,y) L = (x - 1, y)
    move (x,y) R = (x + 1, y)
    moves = concat $ L.unfoldr moreMoves 1
    moreMoves n = let ms = concat [replicate n R
                                  ,replicate n U
                                  ,replicate (n + 1) L
                                  ,replicate (n + 1) D
                                  ]
                   in Just (ms, n + 2)

-- distance from an address to the origin
-- This is slower than the more complex approach used in part 1.
addrDistance :: Addr -> Int
addrDistance = distance origin . (coordinates !!) . subtract 1

stressTest :: Int -> Int
stressTest lim = head $ dropWhile (<= lim) values
  where values = snd $ L.mapAccumL step (M.singleton origin 1) coordinates
        step mem addr = let m = M.alter (sumOf mem $ neighbours addr) addr mem
                         in (m, m M.! addr)
        sumOf _ _ (Just v)   = Just v -- i.e the origin
        sumOf mem ks Nothing = let v = sum (ks >>= \k -> maybeToList (M.lookup k mem))
                                in Just v


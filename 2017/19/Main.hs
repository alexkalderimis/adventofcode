{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Array              as A
import           Data.Attoparsec.Text    (letter)
import qualified Data.List               as L
import           Data.Maybe              (listToMaybe)
import qualified Data.Text               as Text
import           Text.Parser.Char        (newline)
import           Text.Parser.Combinators (choice, sepBy1)

import           Elves
import           Elves.Advent
import qualified Elves.Cartesian         as C
import           Elves.Coord             (manhattan)

data Tile
  = Named Char
  | Empty
  | Turn
  | Horizontal
  | Vertical
  deriving (Show, Eq)

newtype Diagram = Diagram { unDiagram :: A.Array C.Location Tile } deriving (Show, Eq)

main :: IO ()
main = day 19 (followPath <$> parser) pt1 pt2 test
  where
    pt1 = putStrLn . namedTiles
    pt2 = print    . pathLength

test = do
  describe "exampleDiagram" $ do
    let md = parseOnly parser exampleDiagram
    describe "path length" $ do
      it "gets the correct path length" $ do
        let (Right d) = md
        pathLength (followPath d) `shouldBe` 38
    describe "path" $ do
      it "follows the correct path" $ do
        let (Right d) = md
        namedTiles (followPath d) `shouldBe` "ABCDEF"
    describe "start" $ do
      it "finds the correct starting position" $ do
        fmap start md `shouldBe` Right (Just ((0,5),Vertical))
    describe "neighbours" $ do
      let (Right d) = md
      it "knows where to go from A" $ do
        neighbours d (Just (1,5)) (Named 'A') (2,5) `shouldBe` [(3,5)]
      it "knows where to go from E" $ do
        neighbours d (Just (3,12)) (Named 'E') (3,10) `shouldBe` [(3,9)]
      it "knows how to get to E" $ do
        neighbours d (Just (3,13)) Horizontal (3,12) `shouldBe` [(3,10)]
      it "knows where to go from the first turn" $ do
        neighbours d (Just (4,5)) Turn (5,5) `shouldBe` [(5,6)]
      it "knows how to go under wires" $ do
        neighbours d (Just (5,8)) Vertical (4,8) `shouldBe` [(2,8)]

parser = diagram <$> (rowP `sepBy1` newline)
  where
    rowP = some $ choice [Empty      <$ " "
                         ,Turn       <$ "+"
                         ,Horizontal <$ "-"
                         ,Vertical   <$ "|"
                         ,Named      <$> letter
                         ]

namedTiles :: [(C.Location, Tile)] -> [Char]
namedTiles path = [name | (_, Named name) <- path]

pathLength :: [(C.Location, Tile)] -> Int
pathLength [] = 0
pathLength ((loc,_) : path) = snd $ L.foldl' (\(a,n) b -> (b, n + manhattan a b))
                                       (loc, 1)
                                       (fst <$> path)


followPath :: Diagram -> [(C.Location, Tile)]
followPath d = go [] (start d)
  where
    go visited Nothing = reverse visited
    go visited (Just (loc,tile))
      = let previous = fst <$> listToMaybe visited
            next = listToMaybe $ fmap (id <#> (unDiagram d A.!))
                               $ neighbours d previous tile loc
         in go ((loc,tile):visited) next

start :: Diagram -> Maybe (C.Location,Tile)
start (Diagram a) = listToMaybe [(loc,Vertical) | loc <- A.range ((0,0),(0,snd (snd $ A.bounds a)))
                                                , Vertical <- [a A.! loc]
                                ]

neighbours :: Diagram -> Maybe C.Location -> Tile -> C.Location -> [C.Location]
neighbours (Diagram a) p tile loc = acceptable $ case tile of
    Empty      -> []
    Turn       -> adjacent loc
    Horizontal -> [left loc, right loc]
    Vertical   -> [up loc, down loc]
    Named{}    -> case p of
                    Just l | l `horizontal` loc -> [left loc, right loc]
                    Just l | l `vertical` loc -> [up loc, down loc]
                    _      -> adjacent loc
  where
    acceptable              = filter (\l -> maybe True (/= l) p) . isnt Empty
    horizontal (y,_) (y',_) = y == y'
    vertical   (_,x) (_,x') = x == x'
    validTile               = A.inRange (A.bounds a)
    isnt x = filter ((/= x) . (a A.!)) . filter validTile

    -- lr/ud with awareness of where we cannot go
    adjacent loc = isnt Vertical [C.left loc, C.right loc]
                   ++
                   isnt Horizontal [C.up loc, C.down loc]
    -- handle threading under wires
    threading f under loc = let loc' = f loc
                             in if (validTile loc' && a A.! loc' == under)
                                    then threading f under loc'
                                    else loc'
    up    = threading C.up Horizontal
    down  = threading C.down Horizontal
    right = threading C.right Vertical
    left  = threading C.left Vertical


diagram []   = error "No rows"
diagram rows = Diagram $ A.listArray ((0,0), (length rows - 1, rowLen - 1)) (concat $ fmap (padTo rowLen Empty) rows)
  where rowLen = maximum (length <$> rows)

padTo n e es = take n (es ++ repeat e)

exampleDiagram = Text.unlines
--  0123456789012345
  ["     |          " -- 0
  ,"     |  +--+    " -- 1
  ,"     A  |  C    " -- 2
  ," F---|----E|--+ " -- 3
  ,"     |  |  |  D " -- 4
  ,"     +B-+  +--+ " -- 5
  ," "
  ]


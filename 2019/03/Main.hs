{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}

import qualified Data.Array           as A
import qualified Data.Attoparsec.Text as A
import           Data.Foldable        (foldl')
import           Data.Function        (on, (&))
import qualified Data.List            as L
import           Data.Map.Strict      (Map)
import qualified Data.Map.Strict      as Map
import           Data.Maybe
import           Data.Ord             (comparing)
import qualified Data.Set             as Set
import           Data.Text            (Text)
import qualified Data.Text            as T
import           Text.Parser.Char     (char, newline)

import           Elves
import           Elves.Advent
import           Elves.Coord          (manhattan, origin)
import qualified Elves.Coord          as Coord

data Direction = H | V deriving (Show, Eq)

type Command = (Direction, Int)

type Pos = (Int, Int)
type Path = Map Pos Int

data T3 a b c = T3 { t0 :: !a, t1 :: !b, t3 :: !c } deriving (Show, Eq)

main :: IO ()
main = day 03 parser pt1 pt2 test
  where
    parser = (,) <$> (parseDirection <* newline) <*> parseDirection
    pt1 (a, b) = case fst <$> closestCrossing (buildPath a) (buildPath b) of
                   Nothing   -> putStrLn "Lines do not cross"
                   Just dist -> print dist
    pt2 (a, b) = case fst <$> crossingBySignalDelay (buildPath a) (buildPath b) of
                   Nothing    -> putStrLn "Lines do not cross"
                   Just delay -> print delay

parseDirection :: Parser [Command]
parseDirection = direction `A.sepBy1` char ','
  where
    pos c = char c *> A.decimal
    neg c = negate <$> pos c
    direction = fmap (H,) (pos 'R' <|> neg 'L')
                <|>
                fmap (V,) (pos 'U' <|> neg 'D')

buildPath :: [Command] -> Path
buildPath = t3 . foldl' f (T3 0 origin mempty)
  where
    f (T3 steps loc s) cmd = let newLoc = move loc cmd
                                 s' = Map.unionWith const s $ visited steps loc newLoc
                                 steps' = steps + length cmd
                              in T3 steps' newLoc s'
    move (x,y) (H, n) = (x + n, y)
    move (x,y) (V, n) = (x, y + n)
    next a b          = if b > a then a + 1 else a - 1
    fromTo a b        = [a, next a b .. b]
    visited start (x,y) (x', y') = Map.fromList $ zip [(a,b) | a <- fromTo x x', b <- fromTo y y'] [start ..]
    length (_, n) = abs n


bestCrossing :: Ord a => (Pos -> a) -> Path -> Path -> Maybe (a, (Int, Int))
bestCrossing f a b = Set.intersection (Map.keysSet a) (Map.keysSet b)
                   & Set.toList
                   & filter (/= origin)
                   & fmap (\x -> (f x, x))
                   & L.uncons
                   & fmap (L.minimumBy (comparing fst) . uncurry (:))

closestCrossing :: Path -> Path -> Maybe (Int, (Int, Int))
closestCrossing = bestCrossing (manhattan origin)

crossingBySignalDelay :: Path -> Path -> Maybe (Int, (Int, Int))
crossingBySignalDelay a b = bestCrossing (\pos -> sum [s Map.! pos | s <- [a, b]]) a b

test = do
  describe "crossingBySignalDelay" $ do
    let examples = [("R8,U5,L5,D3", "U7,R6,D4,L4", 30)
                   ,("R75,D30,R83,U83,L12,D49,R71,U7,L72","U62,R66,U55,R34,D71,R55,D58,R83", 610)
                   ,("R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51", "U98,R91,D20,R16,D67,R40,U7,R15,U6,R7", 410)
                   ]
    forM_ (zip [1 ..] examples) $ \(i, (path_a, path_b, distance)) ->
      it ("finds the closest crossing point: " <> show i) $ do
        let ret = do a <- buildPath <$> parseOnly parseDirection path_a
                     b <- buildPath <$> parseOnly parseDirection path_b
                     return . fromMaybe 0 . fmap fst $ crossingBySignalDelay a b
        ret `shouldBe` (Right distance)

  describe "closestCrossing" $ do
    let examples = [("R8,U5,L5,D3", "U7,R6,D4,L4", 6)
                   ,("R75,D30,R83,U83,L12,D49,R71,U7,L72","U62,R66,U55,R34,D71,R55,D58,R83", 159)
                   ,("R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51", "U98,R91,D20,R16,D67,R40,U7,R15,U6,R7", 135)
                   ]
    forM_ (zip [1 ..] examples) $ \(i, (path_a, path_b, distance)) ->
      it ("finds the closest crossing point: " <> show i) $ do
        let ret = do a <- buildPath <$> parseOnly parseDirection path_a
                     b <- buildPath <$> parseOnly parseDirection path_b
                     let (x,y) = fromMaybe origin . fmap snd $ closestCrossing a b
                     return (abs x + abs y)
        ret `shouldBe` (Right distance)

  describe "parseDirection" $ do
    let examples =
          [("R75,D30,R83,U83,L12,D49,R71,U7,L72"
            , [(H, 75), (V, -30), (H, 83), (V, 83), (H, -12), (V, -49), (H, 71), (V, 7), (H, -72)])
          ,("U62,R66,U55,R34,D71,R55,D58,R83"
            , [(V, 62), (H, 66), (V, 55), (H, 34), (V, -71), (H, 55), (V, -58), (H, 83)])
          ,("R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51"
            , [(H, 98), (V, 47), (H, 26), (V, -63), (H, 33), (V, 87), (H, -62), (V, -20), (H, 33), (V, 53), (H, 51)])
          ,("U98,R91,D20,R16,D67,R40,U7,R15,U6,R7"
            , [(V, 98), (H, 91), (V, -20), (H, 16), (V, -67), (H, 40), (V, 7), (H, 15), (V, 6), (H, 7)])
          ]
    forM_ examples $ \(text, commands) -> it ("can parse " <> T.unpack text) $ do
      parseOnly parseDirection text `shouldBe` Right commands
  describe "buildPath" $ do
    let examples =
          [([], mempty)
          ,([(H, 5), (V, 5)], Set.fromList ([(x, 0) | x <- [0 .. 5]] <> [(5, y) | y <- [0 .. 5]]))
          ,([(V, 2), (H, 2), (V, -2), (H, -1), (V, 2)], Set.fromList [(x,y) | x <- [0 .. 2], y <- [0 .. 2]])
          ]
    forM_ (zip [1 ..] examples) $ \(i, (path, expected)) -> it ("Can follow path " <> show i) $ do
      Map.keysSet (buildPath path) `shouldBe` expected


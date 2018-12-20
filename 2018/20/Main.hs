{-# LANGUAGE OverloadedStrings #-}

import           Control.Applicative
import           Data.Attoparsec.Text    (Parser, parseOnly)
import qualified Data.List               as L
import qualified Data.Map.Strict         as M
import           Data.Maybe
import           Data.Ord
import           Data.Text               (Text)
import qualified Data.Text               as Text
import qualified Data.Text.IO            as Text
import           Data.Tree
import           System.Exit
import           Text.Parser.Char
import           Text.Parser.Combinators

data Direction = N | S | E | W deriving (Show)
type Building = Tree (Maybe Direction)

type Location = (Int, Int)

main :: IO ()
main = do
  eb <- parseOnly buildingP <$> Text.getContents
  case eb of
    Left err -> die err
    Right b  -> do putStrLn "Parsed"
                   let m = explore b
                   print (maximum . M.elems $ m)
                   print $ length (filter (>= 1000) (M.elems m))

move :: Direction -> Location -> Location
move N = translate (-1,0)
move S = translate ( 1,0)
move W = translate ( 0,-1)
move E = translate ( 0, 1)

translate (dy,dx) (y,x) = (y + dy, x + dx)

maxDepth :: Building -> Int
maxDepth = maximum . M.elems . explore

-- flood-fill the building, marking each room with the shortest path cost
explore :: Building -> M.Map Location Int
explore = go mempty (0,0) (0,0)
  where
    setMin v = pure . maybe v (min v)
    go m parent here b =
      let costToParent = fromMaybe (-1) (M.lookup parent m)
          costToHere = costToParent + 1
          m' = M.alter (setMin costToHere) here m
          nextSteps = [(move dir here, n) | n <- subForest b
                                          , Just dir <- [rootLabel n]
                                          ]
          exploreFurther m (there,node) = go m here there node
       in case M.lookup here m of
            Just cost | cost < costToHere -> m -- we have found a cheaper path. Stop
            _ -> L.foldl' exploreFurther m' nextSteps
                              

buildingP :: Parser Building
buildingP = char '^' *> building Nothing <* char '$'
  where
    building dir = Node dir <$> (concat <$> many door)
    door :: Parser [Building]
    door = choice [fmap pure (simpleDirection >>= building)
                  ,group >>= groupTail
                  ]
    lparen = char '('
    rparen = char ')'
    alternatives = door `sepBy1` char '|'
    group = lparen *> ((,) <$> alternatives <*> optional (char '|')) <* rparen
    -- this way of parsing the groups was vital to avoid pathological backtracking
    groupTail (bs, Nothing) = commonTail bs
    groupTail (bs, _      ) = sharedTail bs
    -- when groups are partially specified, the tail also applies to where we left off
    sharedTail bs = do mds <- optional door
                       case mds of
                         Nothing -> return (concat bs)
                         Just ds -> return ((graft ds <$> concat bs) ++ ds)
    -- when groups are fully specified, then they all restart from the given position
    commonTail bs = do mds <- optional door
                       case mds of
                         Nothing -> return (concat bs)
                         Just ds -> return (graft ds <$> concat bs)
    simpleDirection = choice [Just d <$ string (show d) | d <- [N,S,E,W]]
    -- graft a tail onto the end of a set of paths.
    graft ds b = case subForest b of
                   [] -> b { subForest = ds }
                   sf -> b { subForest = graft ds <$> sf }


examples :: [Text]
examples =
  ["^WNE$"
  ,"^N(E|W)N$"
  ,"^ENWWW(NEEE|SSE(EE|N))$"
  ,"^ENNWSWW(NEWS|)SSSEEN(WNSE|)EE(SWEN|)NNN$"
  ,"^ESSWWN(E|NNENN(EESS(WNSE|)SSS|WWWSSSSE(SW|NNNE)))$"
  ,"^WSSEESWWWNW(S|NENNEEEENN(ESSSSW(NWSW|SSEN)|WSWWN(E|WWS(E|SS))))$"
  ]

runExamples = do
  flip mapM_ examples $ \ex -> do
    Text.putStr ex
    Text.putStr " "
    print (maxDepth <$> parseOnly buildingP ex)

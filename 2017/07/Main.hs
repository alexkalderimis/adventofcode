{-# LANGUAGE TupleSections #-}

import Control.Applicative
import Text.Parser.Combinators (choice, sepBy1)
import Text.Parser.Char
import Text.ParserCombinators.ReadP (readP_to_S, ReadP)
import Data.Semigroup
import Data.Maybe
import Data.Ord
import Data.Function
import Data.Foldable
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as S
import qualified Data.List as L
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import           Data.Text   (Text)
import qualified Data.Tree as Tree
import           Data.Tree (Tree(..))

type Set = S.HashSet
type Name = Text
type Row = (Name, Int, Set Name)

type Tower = Tree (Name, Int)

main :: IO ()
main = do
  strs <- lines <$> getContents
  case traverse parseRow strs of
    Nothing -> error "Could not parse input"
    Just rs -> do
      putStrLn "BASE OF TOWER"
      traverse_ Text.putStrLn (unsupported $ supportedBy rs)
      putStrLn "FIX NEEDED"
      let tower = buildTower rs
      traverse_ print (fixTower tower)

parseRow :: String -> Maybe Row
parseRow = runParser rowP

runParser :: ReadP a -> String -> Maybe a
runParser p = fmap fst . listToMaybe . reverse . readP_to_S p

-- parse one of the rows
rowP :: ReadP Row
rowP = (,,) <$> nameP
            <*> (char ' ' *> weightP)
            <*> choice [ string " -> " *> supportedP
                       , pure mempty]
  where
    nameP      = Text.pack  <$> some lower
    weightP    = read       <$> parens (some digit)
    supportedP = S.fromList <$> sepBy1 nameP (string ", ")
    parens pa = char '(' *> pa <* char ')'

-- build a map from the name of the node to its parent
-- in the tree (i.e. the one that is supporting this node)
supportedBy :: [(Name, a, Set Name)] -> HM.HashMap Name Name
supportedBy 
  = HM.fromList . concatMap (\(below, _, ontop) ->
      concatMap (pure . (,below)) ontop)

-- find the name of the root node
-- We just do this by following any random path up to the root.
unsupported :: HM.HashMap Name Name -> Maybe Name
unsupported hm | HM.null hm = Nothing
unsupported hm = go (head $ HM.toList hm)
  where
    go (ontop, below) = case HM.lookup below hm of
                          Nothing -> Just below
                          Just n  -> go (below, n)

isBalanced :: Tower -> Bool
isBalanced node = case Tree.subForest node of
  [] -> True
  (n:ns) -> all (== weight n) (fmap weight ns)

findFix :: Int -> Tower -> Maybe (Name, Int)
findFix diff n
  | isBalanced n = Just $ fmap (+ diff) (Tree.rootLabel n)
  | otherwise    = case weightedKids n of
     []      -> error "impossible"
     (xs:_)  -> listToMaybe . catMaybes
                 $ fmap (findFix diff . snd) xs

fixTower :: Tower -> Maybe (Name, Int)
fixTower n | isBalanced n = Nothing
fixTower n = case weightedKids n of
     [[x], others] -> let shouldBe = avg $ fmap fst others
                       in findFix (shouldBe - fst x) n
     []      -> error "impossible"
     _       -> error "cannot solve"
  where
    avg ws = sum ws `div` length ws

-- weighs and orders the kids in groups, from
-- smallest group to largest (used to find the odd-one-out)
weightedKids :: Tower -> [[(Int, Tower)]]
weightedKids n = L.sortBy (comparing length)
               . L.groupBy ((==) `on` fst)
               . L.sortBy (comparing fst)
               $ zip (weight <$> Tree.subForest n)
                     (Tree.subForest n)


weight :: Tower -> Int
weight = getSum . foldMap (Sum . snd)

buildTower :: [Row] -> Tower
buildTower rs =
  let supped = supportedBy rs
   in head $ nodes (maybeToList $ unsupported supped)
  where
    indexed = HM.fromList [(name, (w, ns)) | (name, w, ns) <- rs]
    nodes = fmap $ \name -> case HM.lookup name indexed of
        Nothing -> error $ "bad tree: " ++ show name ++ " not found"
        Just (w, ns) -> Node (name, w) (nodes $ S.toList ns)

example :: [String]
example =
  ["pbga (66)"
  ,"xhth (57)"
  ,"ebii (61)"
  ,"havc (66)"
  ,"ktlj (57)"
  ,"fwft (72) -> ktlj, cntj, xhth"
  ,"qoyq (66)"
  ,"padx (45) -> pbga, havc, qoyq"
  ,"tknk (41) -> ugml, padx, fwft"
  ,"jptl (61)"
  ,"ugml (68) -> gyxo, ebii, jptl"
  ,"gyxo (61)"
  ,"cntj (57)"
  ]

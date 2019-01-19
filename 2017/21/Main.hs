{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}

import           Control.Monad
import           Data.Array.Unboxed      (Array)
import qualified Data.Array.Unboxed      as A
import           Data.Bool
import           Data.Hashable           (Hashable (..))
import           Data.HashMap.Strict     (HashMap)
import qualified Data.HashMap.Strict     as M
import qualified Data.List               as L
import           Data.Maybe
import qualified Data.Text               as Text
import           System.Exit             (die)
import           Text.Parser.Char        (newline, text)
import           Text.Parser.Combinators (count, sepBy1)
import           Text.PrettyPrint        (Doc, ($$), ($+$))
import qualified Text.PrettyPrint        as Pretty

import           Elves
import           Elves.Advent

type Map = HashMap

type Offset = (Int,Int)
type Bit = Bool
newtype BitPattern = BitPattern { getPattern :: Array Offset Bit }
  deriving (Show, Eq)

instance Hashable BitPattern where
  hashWithSalt salt (BitPattern pat) = salt `hashWithSalt` A.bounds pat
                                            `hashWithSalt` A.elems pat

data Rule = Rule BitPattern BitPattern
  deriving (Show)

type RuleSet = Map BitPattern BitPattern

main :: IO ()
main = day 21 (ruleSet <$> parser) pt1 pt2 test
  where
    pt1 = run 5
    pt2 = run 18
    run n rs = do pat0 <- init
                  let pat5 = applyN n (tick rs) pat0
                  print (pixelsOn pat5)
    init = either die pure $ parseOnly (bitPattern 3) startPattern

test = do
  let ix :: Int -> Int -> (Int,Int)
      ix = (,)
  describe "pt1" $ do
    let mrs  = ruleSet <$> parseOnly parser exampleRules
        mpat = parseOnly (bitPattern 3) startPattern
    it "has 12 pixels on after 2 iterations" $ do
      let mret = applyN 2 <$> (tick <$> mrs) <*> mpat
      fmap pixelsOn mret `shouldBe` Right 12

  describe "offset" $ do
    let examples = [ (2, ix 0 0, ix 0 0)
                   , (2, ix 0 1, ix 0 1)
                   , (2, ix 1 0, ix 1 0)
                   , (2, ix 1 1, ix 1 1)

                   , (2, ix 0 2, ix 0 0)
                   , (2, ix 0 3, ix 0 1)
                   , (2, ix 1 2, ix 1 0)
                   , (2, ix 1 3, ix 1 1)

                   , (2, ix 2 0, ix 0 0)
                   , (2, ix 2 1, ix 0 1)
                   , (2, ix 3 0, ix 1 0)
                   , (2, ix 3 1, ix 1 1)

                   , (2, ix 2 2, ix 0 0)
                   , (2, ix 2 3, ix 0 1)
                   , (2, ix 3 2, ix 1 0)
                   , (2, ix 3 3, ix 1 1)

                   , (3, ix 0 0, ix 0 0)
                   , (3, ix 0 1, ix 0 1)
                   , (3, ix 0 2, ix 0 2)
                   , (3, ix 1 0, ix 1 0)
                   , (3, ix 1 1, ix 1 1)
                   , (3, ix 1 2, ix 1 2)
                   , (3, ix 2 0, ix 2 0)
                   , (3, ix 2 1, ix 2 1)
                   , (3, ix 2 2, ix 2 2)

                   , (3, ix 0 3, ix 0 0)
                   , (3, ix 0 4, ix 0 1)
                   , (3, ix 0 5, ix 0 2)
                   , (3, ix 1 3, ix 1 0)
                   , (3, ix 1 4, ix 1 1)
                   , (3, ix 1 5, ix 1 2)
                   , (3, ix 2 3, ix 2 0)
                   , (3, ix 2 4, ix 2 1)
                   , (3, ix 2 5, ix 2 2)

                   , (3, ix 3 0, ix 0 0)
                   , (3, ix 3 1, ix 0 1)
                   , (3, ix 3 2, ix 0 2)
                   , (3, ix 4 0, ix 1 0)
                   , (3, ix 4 1, ix 1 1)
                   , (3, ix 4 2, ix 1 2)
                   , (3, ix 5 0, ix 2 0)
                   , (3, ix 5 1, ix 2 1)
                   , (3, ix 5 2, ix 2 2)

                   , (3, ix 3 3, ix 0 0)
                   , (3, ix 3 4, ix 0 1)
                   , (3, ix 3 5, ix 0 2)
                   , (3, ix 4 3, ix 1 0)
                   , (3, ix 4 4, ix 1 1)
                   , (3, ix 4 5, ix 1 2)
                   , (3, ix 5 3, ix 2 0)
                   , (3, ix 5 4, ix 2 1)
                   , (3, ix 5 5, ix 2 2)
                   ]
    forM_ examples $ \inp ->
      specify (show inp) $ let (n,ix,res) = inp in offset n ix `shouldBe` res

  describe "chunkIx" $ do
    let examples = [ (2, ix 0 0, ix 0 0)
                   , (2, ix 0 1, ix 0 0)
                   , (2, ix 1 0, ix 0 0)
                   , (2, ix 1 1, ix 0 0)

                   , (2, ix 0 2, ix 0 1)
                   , (2, ix 0 3, ix 0 1)
                   , (2, ix 1 2, ix 0 1)
                   , (2, ix 1 3, ix 0 1)

                   , (2, ix 2 0, ix 1 0)
                   , (2, ix 2 1, ix 1 0)
                   , (2, ix 3 0, ix 1 0)
                   , (2, ix 3 1, ix 1 0)

                   , (2, ix 2 2, ix 1 1)
                   , (2, ix 2 3, ix 1 1)
                   , (2, ix 3 2, ix 1 1)
                   , (2, ix 3 3, ix 1 1)

                   , (3, ix 0 0, ix 0 0)
                   , (3, ix 0 1, ix 0 0)
                   , (3, ix 0 2, ix 0 0)
                   , (3, ix 1 0, ix 0 0)
                   , (3, ix 1 1, ix 0 0)
                   , (3, ix 1 2, ix 0 0)
                   , (3, ix 2 0, ix 0 0)
                   , (3, ix 2 1, ix 0 0)
                   , (3, ix 2 2, ix 0 0)

                   , (3, ix 0 3, ix 0 1)
                   , (3, ix 0 4, ix 0 1)
                   , (3, ix 0 5, ix 0 1)
                   , (3, ix 1 3, ix 0 1)
                   , (3, ix 1 4, ix 0 1)
                   , (3, ix 1 5, ix 0 1)
                   , (3, ix 2 3, ix 0 1)
                   , (3, ix 2 4, ix 0 1)
                   , (3, ix 2 5, ix 0 1)

                   , (3, ix 3 0, ix 1 0)
                   , (3, ix 3 1, ix 1 0)
                   , (3, ix 3 2, ix 1 0)
                   , (3, ix 4 0, ix 1 0)
                   , (3, ix 4 1, ix 1 0)
                   , (3, ix 4 2, ix 1 0)
                   , (3, ix 5 0, ix 1 0)
                   , (3, ix 5 1, ix 1 0)
                   , (3, ix 5 2, ix 1 0)

                   , (3, ix 3 3, ix 1 1)
                   , (3, ix 3 4, ix 1 1)
                   , (3, ix 3 5, ix 1 1)
                   , (3, ix 4 3, ix 1 1)
                   , (3, ix 4 4, ix 1 1)
                   , (3, ix 4 5, ix 1 1)
                   , (3, ix 5 3, ix 1 1)
                   , (3, ix 5 4, ix 1 1)
                   , (3, ix 5 5, ix 1 1)
                   ]
    forM_ examples $ \inp ->
      specify (show inp) $ let (n,ix,res) = inp in chunkIx n ix `shouldBe` res

chunkIx :: Int -> Offset -> Offset
chunkIx n (y,x) = (y `div` n, x `div` n)

offset :: Int -> Offset -> Offset
offset n (y,x) = (y `mod` n, x `mod` n)

-- the rule-set is expected to include one pattern for any possible key.
getChunks :: RuleSet -> Int -> BitPattern -> Map Offset BitPattern
getChunks rules n (BitPattern pat) = M.fromList [ (ix, chunk ix) | ix <- A.range bs ]
  where
    -- the chunk indices
    bs        = ((0,0), chunkIx n . snd $ A.bounds pat)
    -- the chunk for that index
    chunk ix  = rules M.! BitPattern (key ix)
    -- the slice of the input pattern from (y * n, x * m) to (y * n + m, x * n + m)
    key (y,x) = let topLeft = (y * n, x * n)
                    m = n - 1
                 in A.ixmap ((0,0), (m,m)) (\(yy,xx) -> (fst topLeft + yy, snd topLeft + xx)) pat

tick :: RuleSet -> BitPattern -> BitPattern
tick rules (BitPattern a) = BitPattern $ A.listArray bs (bit <$> A.range bs)
  where
    chunks = getChunks rules divisor (BitPattern a)
    bs = ((0,0), (newSize - 1, newSize - 1))
    bit ix = let groupIx = chunkIx newGrpSize ix
                 ix'     = offset newGrpSize ix
                 BitPattern chunk = fromMaybe (error $ "no chunk " ++ show groupIx
                                                     ++ ", keys = " ++ show (M.keys chunks)
                                                     ++ ", newGrpSize = " ++ show newGrpSize
                                                     ++ ", ix' = " ++ show ix'
                                              )
                                    $ M.lookup groupIx chunks
              in chunk A.! ix'
    newSize = n * newGrpSize
    n       = oldSize `div` divisor
    oldSize = (+ 1) . fst . snd $ A.bounds a
    newGrpSize = case divisor of
                   2 -> 3
                   3 -> 4
                   _ -> error $ "unexpected divisor value: " ++ show divisor
    divisor    = case (oldSize `mod` 2, oldSize `mod` 3) of
                   (0,_) -> 2
                   (_,0) -> 3
                   _     -> error $ show oldSize ++ " is not divisible by 2 or 3"


ruleSet :: [Rule] -> RuleSet
ruleSet rules = M.fromList [(lhs pat, rhs)
                                      | (Rule pat rhs) <- rules
                                      , flip <- [id, flipVertical, flipHorizontal]
                                      , turn <- [applyN 0 rotate, applyN 1 rotate, applyN 2 rotate, applyN 3 rotate]
                                      , let lhs = flip . turn
                           ]

flipVertical :: BitPattern -> BitPattern
flipVertical (BitPattern a)
  = let bs = A.bounds  a
        (y',_) = snd bs
    in BitPattern $ A.ixmap bs (\(y,x) -> (y' - y, x)) a

flipHorizontal :: BitPattern -> BitPattern
flipHorizontal (BitPattern a)
  = let bs = A.bounds a
        (_,x') = snd bs
    in BitPattern $ A.ixmap bs (\(y,x) -> (y, x' - x)) a

rotate :: BitPattern -> BitPattern
rotate (BitPattern a)
  = let bs = A.bounds a
        (y',x') = snd bs
     in BitPattern . A.listArray bs
                   . concat
                   . reverse
                   $ L.transpose  [[a A.! (y,x) | x <- [0 .. x']]
                                                | y <- [0 .. y']
                                                ]

prettyRule :: Rule -> Pretty.Doc
prettyRule (Rule lhs rhs) = (pat lhs) $$ (Pretty.nest 5  "=>") $$ (Pretty.nest 8 (pat rhs))

pat :: BitPattern -> Pretty.Doc
pat (BitPattern a) = Pretty.vcat (row <$> [0 .. y'])
  where
    (y',x') = snd $ A.bounds a
    row y = Pretty.hcat $ fmap (bool "." "#" . (a A.!) . (y,)) [0 .. x']

parser :: Parser [Rule]
parser = ruleP `sepBy1` newline
  where
    ruleP = rule3 <|> rule2
    rule2 = Rule <$> bitPattern 2 <*> (" => " *> bitPattern 3)
    rule3 = Rule <$> bitPattern 3 <*> (" => " *> bitPattern 4)

bitPattern :: Int -> Parser BitPattern
bitPattern n = do
  row0 <- row n
  rows <- count (n - 1) (text "/" *> row n)
  return . BitPattern $ A.listArray ((0,0),(n-1,n-1)) (row0 ++ concat rows)
 where
  row n = count n ((True <$ text "#") <|> (False <$ text "."))

exampleRules = Text.unlines
  ["../.# => ##./#../..."
  ,".#./..#/### => #..#/..../..../#..#"
  ]

check = do
   let (Right rs) = parseOnly parser exampleRules
   let rules = ruleSet rs
   let (Right pat0) = parseOnly (bitPattern 3) startPattern
   let pat1 = tick rules pat0
   putStrLn . Pretty.render $ pat pat1
   let pat2 = tick rules pat1
   putStrLn . Pretty.render $ pat pat2

pixelsOn :: BitPattern -> Int
pixelsOn (BitPattern a) = length . filter id $ A.elems a

applyN :: Int -> (a -> a)  -> a -> a
applyN n f = L.foldl' (.) id (replicate n f)

startPattern :: Text.Text
startPattern = Text.intercalate "/"
  [".#."
  ,"..#"
  ,"###"
  ]

tickedPattern :: Text.Text
tickedPattern = Text.intercalate "/"
  ["#..#"
  ,"...."
  ,"...."
  ,"#..#"
  ]

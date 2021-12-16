{-# LANGUAGE OverloadedStrings #-}
import qualified Data.List.Extra as L
import qualified Data.Text as T
import           Data.Text (Text)
import qualified Data.Attoparsec.Text as A
import qualified Data.Array.Unboxed as Array
import           Data.Array.Unboxed (UArray, (!))
import qualified Data.Set as Set
import           Data.Set (Set)

import Elves
import Elves.Advent
import Elves.StrictGrid
import qualified Elves.AStar as AS

type Risk = Int
newtype Grid = Grid { ungrid :: UArray Coord Risk } deriving (Eq)

instance Show Grid where
  show (Grid g)
    = let (_, ub) = Array.bounds g
          w = getCol (col ub) + 1
      in L.intercalate "\n" . fmap mconcat . L.chunksOf w . fmap show $ Array.elems g

main :: IO ()
main = day 15 parser pt1 pt2 test
  where
    pt1 g = maybe (error "no path found") pure (bestPath g) >>= print . pathRisk g
    pt2 g = let g' = fullMap g
             in maybe (error "no path found") pure (bestPath g') >>= print . pathRisk g'

parser :: Parser Grid
parser = Grid <$> gridP (read . pure <$> A.digit)

bestPath :: Grid -> Maybe [Coord]
bestPath (Grid g)
  = let (start, goal) = Array.bounds g
    in AS.aStarOrd
       (Set.fromList . nextCoords False (Array.bounds g))
       (\_ b -> g ! b)
       (manhattan goal)
       (== goal)
       start

pathRisk :: Grid -> [Coord] -> Risk
pathRisk (Grid g) = sum . fmap (g !)

fullMap :: Grid -> Grid
fullMap (Grid g)
  = let (lb, ub) = Array.bounds g
        w = getCol (col ub) + 1
        h = getRow (row ub) + 1
        d = Delta (4 * h) (4 * w)
        ub' = move ub d
        extend = take 5 . iterate bump -- turn [a] into [a][b][c][d][e]
        offset = zip [0..]             -- assign an offset
        withOffsets = offset . fmap (offset . extend) . extend . Grid
     in Grid . Array.array (lb, ub') $ do
          (col_offset, col)        <- withOffsets g
          (row_offset, Grid array) <- col
          (i, risk)                <- Array.assocs array
          let delta = Delta (row_offset * h) (col_offset * h)
          pure (move i delta, risk)

bump :: Grid -> Grid
bump = Grid . Array.amap (atLeast 1 . (`mod` 10) . succ) . ungrid

test = do
  let exampleInput = [ "1163751742"
                     , "1381373672"
                     , "2136511328"
                     , "3694931569"
                     , "7463417111"
                     , "1319128137"
                     , "1359912421"
                     , "3125421639"
                     , "1293138521"
                     , "2311944581"
                     ]
  let Right grid = parseOnly parser (T.intercalate "\n" exampleInput)

  describe "bestPath" $ do
    let Just path = bestPath grid
    it "finds the optimal path" $ do
      let risk = let Grid g = grid in (g !)
      fmap risk path `shouldBe` [1, 2, 1, 3, 6, 5, 1, 1, 1, 5, 1, 1, 3, 2, 3, 2, 1, 1]
    it "knows the overall risk" $ do
      pathRisk grid path `shouldBe` 40

  describe "bump" $ do
    let Right bumped = parseOnly parser $ T.intercalate "\n"
                                        [ "2274862853"
                                        , "2492484783"
                                        , "3247622439"
                                        , "4715142671"
                                        , "8574528222"
                                        , "2421239248"
                                        , "2461123532"
                                        , "4236532741"
                                        , "2314249632"
                                        , "3422155692"
                                        ]
    it "bumps a grid correctly" $ do
      bump grid `shouldBe` bumped

  describe "fullMap" $ do
    let expanded = [ "11637517422274862853338597396444961841755517295286"
                   , "13813736722492484783351359589446246169155735727126"
                   , "21365113283247622439435873354154698446526571955763"
                   , "36949315694715142671582625378269373648937148475914"
                   , "74634171118574528222968563933317967414442817852555"
                   , "13191281372421239248353234135946434524615754563572"
                   , "13599124212461123532357223464346833457545794456865"
                   , "31254216394236532741534764385264587549637569865174"
                   , "12931385212314249632342535174345364628545647573965"
                   , "23119445813422155692453326671356443778246755488935"

                   , "22748628533385973964449618417555172952866628316397"
                   , "24924847833513595894462461691557357271266846838237"
                   , "32476224394358733541546984465265719557637682166874"
                   , "47151426715826253782693736489371484759148259586125"
                   , "85745282229685639333179674144428178525553928963666"
                   , "24212392483532341359464345246157545635726865674683"
                   , "24611235323572234643468334575457944568656815567976"
                   , "42365327415347643852645875496375698651748671976285"
                   , "23142496323425351743453646285456475739656758684176"
                   , "34221556924533266713564437782467554889357866599146"

                   , "33859739644496184175551729528666283163977739427418"
                   , "35135958944624616915573572712668468382377957949348"
                   , "43587335415469844652657195576376821668748793277985"
                   , "58262537826937364893714847591482595861259361697236"
                   , "96856393331796741444281785255539289636664139174777"
                   , "35323413594643452461575456357268656746837976785794"
                   , "35722346434683345754579445686568155679767926678187"
                   , "53476438526458754963756986517486719762859782187396"
                   , "34253517434536462854564757396567586841767869795287"
                   , "45332667135644377824675548893578665991468977611257"

                   , "44961841755517295286662831639777394274188841538529"
                   , "46246169155735727126684683823779579493488168151459"
                   , "54698446526571955763768216687487932779859814388196"
                   , "69373648937148475914825958612593616972361472718347"
                   , "17967414442817852555392896366641391747775241285888"
                   , "46434524615754563572686567468379767857948187896815"
                   , "46833457545794456865681556797679266781878137789298"
                   , "64587549637569865174867197628597821873961893298417"
                   , "45364628545647573965675868417678697952878971816398"
                   , "56443778246755488935786659914689776112579188722368"

                   , "55172952866628316397773942741888415385299952649631"
                   , "57357271266846838237795794934881681514599279262561"
                   , "65719557637682166874879327798598143881961925499217"
                   , "71484759148259586125936169723614727183472583829458"
                   , "28178525553928963666413917477752412858886352396999"
                   , "57545635726865674683797678579481878968159298917926"
                   , "57944568656815567976792667818781377892989248891319"
                   , "75698651748671976285978218739618932984172914319528"
                   , "56475739656758684176786979528789718163989182927419"
                   , "67554889357866599146897761125791887223681299833479"
                   ]
    let Right grid' = parseOnly parser (T.intercalate "\n" expanded)

    it "can expand the grid properly" $ do
      fullMap grid `shouldBe` grid'
    it "can find the best path through this grid" $ do
      let Just p = bestPath grid'
      pathRisk grid' p `shouldBe` 315

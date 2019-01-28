{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Elves.RTreeSpec (spec) where

import Prelude hiding (lookup)

import Control.Arrow (first)
import Control.Category ((>>>))
import           Control.Concurrent       (threadDelay)
import qualified Control.Concurrent.Async as Async
import           Control.Exception.Base   (Exception)
import           Data.Functor.Compose
import           Control.Lens             hiding (index)
import qualified Data.Foldable            as F
import qualified Data.Ix                  as Ix
import qualified Data.List                as L
import qualified Data.List.NonEmpty       as NE
import           Data.List.NonEmpty       (NonEmpty(..))
import           Test.Hspec
import           Test.QuickCheck          hiding (within)
import qualified Test.QuickCheck          as QC
import qualified Test.QuickCheck.Monadic  as QCM

import           Elves
import           Elves.Coord
import           Elves.LawsSpec 
import           Elves.RTree              hiding (null)
import qualified Elves.RTree              as RT

type Dim3 = (Int,Int,Int)
type Dim3Set = RTree (Int,Int,Int) ()

newtype Unique a = Unique { getUnique :: [a] } deriving (Show)

instance (Arbitrary a, Ord a) => Arbitrary (Unique a) where
  arbitrary = Unique . L.nub . getSorted <$> arbitrary

newtype Cube = Cube { getCube :: ((Int,Int,Int), (Int,Int,Int)) } deriving (Show, Eq)

cubeSize :: Cube -> Int
cubeSize (Cube bs) = Ix.rangeSize bs

data Heuristic = Euclidean | Manhattan deriving (Show, Eq, Bounded, Enum)

instance Arbitrary Heuristic where
  arbitrary = arbitraryBoundedEnum

measure :: Heuristic -> Dim3 -> Dim3 -> Double
measure Euclidean = straightLine
measure Manhattan = (realToFrac .) . manhattan

instance Arbitrary Cube where
  arbitrary = do
    (a,b,c) <- arbitrary
    a' <- arbitrary `suchThat` (>= a)
    b' <- arbitrary `suchThat` (>= b)
    c' <- arbitrary `suchThat` (>= c)
    return (Cube ((a,b,c),(a',b',c')))

  shrink c = let (Cube (lb,ub)) = c
              in filter (/= c)
                 $ fmap (\d -> let (a,b) = makeCloser (lb ^. runLens d) (ub ^. runLens d)
                               in Cube (set (runLens d) a lb, set (runLens d) b ub))
                   dimensions

data CubeWithPoint = CubeWithPoint Cube (Int,Int,Int) deriving (Show)

instance Arbitrary CubeWithPoint where
  arbitrary = do
    c <- arbitrary
    p <- QC.elements (Ix.range (getCube c))
    return (CubeWithPoint c p)

  shrink (CubeWithPoint c p) = [CubeWithPoint shrunk p | shrunk <- shrink c
                                                       , Ix.inRange (getCube shrunk) p
                               ]

data CubeWithCube = CubeWithCube Cube Cube deriving (Show)

instance Arbitrary CubeWithCube where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary `suchThat` within a
    return (CubeWithCube (Cube a) (Cube b))
  shrink (CubeWithCube a b) = [CubeWithCube (Cube a') (Cube b') | (Cube a') <- shrink a
                                                                , (Cube b') <- shrink b
                                                                , a' `within` b'
                              ]

data NNInput a = NNInput a a [a] deriving Show

instance (Arbitrary a, Ord a) => Arbitrary (NNInput a) where
  arbitrary = do
    Unique xs <- arbitrary
    x <- arbitrary `suchThat` (not . (`elem` xs))
    y <- arbitrary `suchThat` (not . (`elem` (x:xs)))
    return (NNInput x y xs)
  shrink (NNInput a b cs) = NNInput a b <$> shrink cs

makeCloser :: Int -> Int -> (Int,Int)
makeCloser a b = case b - a of
  0 -> (a,b)
  1 -> (a + 1, b)
  _ -> (a + 1, b - 1)

query1 :: Dim3 -> Dim3Set -> [(Dim3,())]
query1 i t = fmap (first fst) . take 1 $ query Within (i,i) t

subregions :: RTree i a -> [RTree i a]
subregions (Region _ ts) = NE.toList ts
subregions _             = []

maxRegionSize :: RTree i a -> Int
maxRegionSize t = let rs = subregions t
                   in maximum (length rs : fmap maxRegionSize rs)

tree :: [Dim3] -> Dim3Set
tree = index . flip zip (repeat ()) . fmap dbl

depth :: RTree i a -> Int
depth Tip = 0
depth Leaf{} = 1
depth (Region _ ts) = 1 + maximum (depth <$> ts)

dbl :: a -> (a,a)
dbl = (,) <$> id <*> id

spec :: Spec
spec = describe "Elves.RTree" $ parallel $ do
  let index' = index . fmap (first dbl)
  describe "size" $ do
    it "always has the size of the elements you put in it" $ property $ \(Unique elems) ->
      let t = index' elems
       in size (t :: Dim3Set) == length elems

  describe "expandB" $ do
    it "is commutative" $ property $ \(Cube a) (Cube b) -> expandB a b === expandB b a

  describe "query" $ do
    it "no-false-positives" $ property $ \e elems ->
      let t = tree $ filter (/= e) elems
       in query1 e t === []
    it "no-false-negatives-insertPoint" $ property $ \e elems ->
      let t = tree elems
       in query1 e (insertPoint e () t) === [(e,())]
    it "no-false-negatives" $ property $ \e elems ->
      let t = index' (e : elems)
       in query1 (fst e) t === [e]
    describe "any strategy" $ do
      it "no-false-negatives" $ property $ \strategy e elems ->
        let t = index [(getCube x, ()) | x <- e : elems ]
         in (getCube e, ()) `elem` query strategy (getCube e) t

    describe "QueryStrategy" $ do
      let a = Leaf (0, 10) 'A'
          b = Leaf (5, 15) 'B'
          c = Leaf (5, 10) 'C'
          d = Leaf (20,30) 'D'
          e = Leaf (-5,-1) 'E'
          f = Leaf (23,27) 'F'
          t = mconcat [a,b,c,d,e,f :: RTree Int Char]
      let search i s = query s i t
          shouldFind x = (`shouldContain` [x]) . fmap snd
          shouldNotFind x = (`shouldNotContain` [x]) . fmap snd
      consider Precisely $ do
        forM_ [a,b,c,d,e,f] $ \(Leaf i a) -> do
          which (show i <> " matches only itself") (search i >>> (`shouldBe` [(i,a)]))
      consider Within $ do
        forM_ [a,b,c,d,e,f] $ \(Leaf i a) -> do
          which (show i <> " matches at least itself") (search i >>> shouldFind a)
        which "finds f inside d"         (search (20,30) >>> shouldFind 'F')
        which "does not find a inside d" (search (20,30) >>> shouldNotFind 'A')
        which "finds c inside a"         (search ( 0,10) >>> shouldFind 'C')
        which "does not find b inside a" (search ( 0,10) >>> shouldNotFind 'B')
        which "does not find d inside a" (search ( 0,10) >>> shouldNotFind 'D')
      consider Overlapping $ do
        forM_ [a,b,c,d,e,f] $ \(Leaf i x) -> do
          which (show i <> " matches at least itself") (search i >>> shouldFind x)
        forM_ (pairs [a,b,c]) $ \(Leaf i x, Leaf _ y) -> do
          which ("finds " <> [y] <> " overlapping " <> [x]) (search i >>> shouldFind y)
        forM_ [a,b,c,d,f] $ \(Leaf i x) -> do
          which ("does not find E overlapping " <> [x]) (search i >>> shouldNotFind 'E')

    describe "lookup" $ do
      specify "We can find anything present in the tree" $ property $ \(Cube bs) x t ->
        lookup bs (Leaf bs x <> t) === Just (x :: Word)
      specify "We cannot find anything not present in the tree" $ property $ \(Cube bs) xs ->
        let t = index (filter ((/= bs) . fst) xs)
         in lookup bs t === (Nothing :: Maybe Word)

    it "expanding a query never makes it less specific" $ property $ \e elems ->
      let t = index' (e : elems)
       in (first dbl e) `elem` query Within (expandQuery 3 $ (fst e, fst e)) (t :: Dim3Set)
    it "can find the midpoint in this line" $ do
      let t = Region ((0,-4,0,0),(0,4,0,0)) (NE.fromList [Leaf (dbl (0,0,0,0)) ()
                                                         ,Leaf (dbl (0,4,0,0)) ()
                                                         ,Leaf (dbl (0,-4,0,0)) ()
                                                         ])
      query Within ((-3,-3,-3,-3),(3,3,3,3)) t `shouldSatisfy` elem (dbl (0,0,0,0), ())


  describe "within" $ do
    specify "all cubes that are within other cubes also overlap" $ property $ \(CubeWithCube (Cube a) (Cube b)) ->
      overlaps a b
    specify "all points in cube are entirely within it" $ property $ \(CubeWithPoint cube p) ->
      (p,p) `within` getCube cube

    it "knows that (-3,-3,-3,-3),(3,3,3,3) is not within (0,-4,0,0),(0,4,0,0)" $ do
      ((0,-4,0),(0,4,0)) `shouldNotSatisfy` within ((-3,-3,-3),(3,3,3))
    it "knows that (0,-4,0,0),(0,4,0,0) is not within (-3,-3,-3,-3),(3,3,3,3)" $ do
      ((-3,-3,-3),(3,3,3)) `shouldNotSatisfy` within ((0,-4,0),(0,4,0))

  describe "expandQuery" $ do
    it "always includes the query" $ property $ \(NonNegative n) q ->
      Ix.inRange (expandQuery n (q,q :: Dim3)) q

  describe "nearestNeighbour" $ do
    specify "in a tree of size 2, one point is always the NN of the other"
      $ property $ \h a b -> 
        (a == b .||. nearestNeighbour (measure h) a (tree [a,b]) == Just (dbl b,()))
    specify "in any tree, for any heuristic no point is closer than the NN"
      $ property $ \h (NNInput x y others) ->
        let f = measure h
            points = x : y : others
            t = tree points
            mnn = nearestNeighbour f x t
         in maybe False ((f x y >=) . f x . fst . fst) mnn

  describe "nearestNeighbourK" $ do
    specify "it returns values in ascending order" $ property $ \h (NonNegative k) p t ->
      let matches = mindist p . fst <$> nearestNeighbourK (measure h) k p (t :: Dim3Set)
       in and [ a <= b | (a,b) <- zip matches (tail matches) ]

  describe "insert" $ do

    describe "duplicate entry" $ do
      let a = Region ( 0,3) (Leaf ( 0,1) True  :| [Leaf (2,3) False])
          b = Region (-1,3) (Leaf (-1,2) False :| [Leaf (2,3) True])
      it "does not exist" $ do
        size (a <> b :: RTree Int Bool) `shouldBe` 3
      it "has the value of the LHS when the LHS is a" $ do
        lookup (2,3) (a <> b) `shouldBe` lookup (2,3) a
      it "has the value of the LHS when the LHS is b" $ do
        lookup (2,3) (b <> a) `shouldBe` lookup (2,3) b
      describe "deeply nested entries" $ do
        let d1_tree = index . flip zip (repeat ()) . fmap pair
            t1 = d1_tree [(-15,-10),(0,5),(4,7),( 9,18),(10,20),(15,16),(17,25)] 
            t2 = d1_tree [(-15, -9),(1,6),(5,8),(10,19),(11,21),(15,16),(18,26)] 
        specify "<> acts like set-union" $ do
          size (t1 <> t2) === (size t1) + (size t2) - 1

    describe "counter-example-1" $ do
      let a = Region (-2,3) (Leaf (-2,1) False :| [Leaf (3,3) True])
          b = Region (0,4) (Leaf (0,3) True :| [Leaf (0,4) False])
      specify "We can combine these regions" $ QC.within 100000 $ do
        size (a <> b :: RTree Int Bool) === 4
      describe "minimal-test-case" $ do
        let mtc = compact $ Region (-2,4) $ sortKids $ insertChild pure (3,3) True
                          $ (Leaf (-2,1) False :| [Leaf (0,3) True, Leaf (0,4) False])
        it "completes successfully" $ QC.within 100000 $ do
          size (mtc :: RTree Int Bool) === 4

    specify "nested-objects" $ QC.within 1000 $ do
      size (insertPoint (0,0,0) () $ insert ((-10,-10,-10),(10,10,10)) () RT.empty) `shouldBe` 2

    -- 000000000011111111112
    -- 012345678901234567890
    -- +-------+ +---------+ 0
    -- |A      | |B        | 1
    -- |     +---------+   | 2
    -- |  +-------------+  | 3
    -- +--|F            |--+ 4
    -- +--|             |--+ 5
    -- |  +-------------+  | 6
    -- |     |E        |   | 7
    -- |C    +---------+  D| 8
    -- +----------+ +------+ 9
    describe "a severely overlapping case" $ do
      let a = Leaf (( 0,  0), ( 4,  8)) 'A'
          b = Leaf (( 0, 10), ( 4, 20)) 'B'
          c = Leaf (( 5,  0), ( 9, 11)) 'C'
          d = Leaf (( 5, 13), ( 9, 20)) 'D'
          e = Leaf (( 2,  6), ( 8, 16)) 'E'
          f = Leaf (( 3,  3), ( 6, 17)) 'F'
      forM_ [a,b,c,d] $ flip consider $ do
        which "overlaps e" (`shouldSatisfy` overlapping e)
        which "overlaps f" (`shouldSatisfy` overlapping f)
      consider f $ do
        which "overlaps a" (`shouldSatisfy` overlapping a)
        which "overlaps b" (`shouldSatisfy` overlapping b)
        which "overlaps c" (`shouldSatisfy` overlapping c)
        which "overlaps d" (`shouldSatisfy` overlapping d)
        which "overlaps e" (`shouldSatisfy` overlapping e)
      it "can combine these leaves" $ property $ do
        mconcat [a,b,c,d,e,f] `shouldSatisfy` ((6 ==) . size)
      it "can combine these leaves-depth" $ property $ do
        mconcat [a,b,c,d,e,f] `shouldSatisfy` ((3 ==) . depth)

    it "increases-size" $ QC.within 100000 $ \t i -> do
      let delta = maybe 1 (pure 0) (lookup (i,i) t)
          t' = insertPoint (i :: Dim3) () t
      size t + delta `shouldBe` size t'
    specify "makes-queries-work" $ QC.within 100000 $ \t i ->
      query1 i (insertPoint i () t) == [(i :: Dim3,())]
                    
    describe "sub-regions" $ do
      let t = Region (1,10) $ NE.fromList [ Region (1,3)  $ NE.fromList [Leaf (dbl 1) (), Leaf (dbl 3) ()]
                                          , Region (8,10) $ NE.fromList [Leaf (dbl 8) (), Leaf (dbl 10) ()]
                                          ]
      it "does not add a new direct child if it is contained by a sub-region" $ do
        let t' = insertPoint (2 :: Int) () t
        length (subregions t') `shouldBe` 2
      it "does add a new direct child if it is not contained by a sub-region" $ do
        let t' = insertPoint (5 :: Int) () t
        length (subregions t') `shouldBe` 3

  describe "maxPageSize" $ do
    let maxRegionSize t = case t of Region _ ts -> maximum (NE.cons (length ts) (fmap maxRegionSize ts))
                                    _ -> 0

    specify "after indexing, no region is larger than the max-page-size" $ property $ \t ->
      maxRegionSize (t :: Dim3Set) <= maxPageSize
    specify "after inserting, no region is larger than the max-page-size" $ property $ \(NonEmpty elems) ->
      let t = foldr (\i -> insertPoint (i :: Dim3) ()) Tip elems
       in maxRegionSize t <= maxPageSize

  describe "null" $ do
    specify "null lists make null trees" $ property $ \ps ->
      RT.null (tree ps) == null ps

  describe "delete" $ do
    it "reduces tree size" $ property $ \p ps ->
      let t = tree (p:ps)
       in size t > size (delete (dbl p) t)
    it "makes points impossible to find" $ property $ \p ps ->
      null (query1 p . delete (dbl p) $ tree (p:ps))

  describe "sizeWith" $ do
    specify "is always extent t when adding a Tip" $ property $ \t ->
      sizeWith t RT.empty == extent (t :: Dim3Set)
    specify "is always >= extent t when adding a tree" $ property $ \t0 t1 ->
      sizeWith t0 t1 >= extent (t0 :: Dim3Set)
    specify "can calculate the new size" $ do
      sizeWith (Leaf (dbl (9,9,9)) ()) (tree [(0,0,0),(3,1,5)]) `shouldBe` 1000

  describe "insertWith" $ do
    specify "it can operate as counting structure" $ property $ do
      let t = L.foldl' (\t p -> RT.insertWith (+) (dbl (p :: Dim3)) (1 :: Int) t) RT.empty
                   [(0,0,0),(0,0,1),(0,1,0),(1,2,1)
                   ,(0,0,0),(0,0,1),(0,1,0)
                           ,(0,0,1),(0,1,0)
                           ,(0,0,1)
                   ]
      L.sort (fmap (first fst) $ assocs t) `shouldBe` L.sort [((0,0,0), 2)
                                            ,((0,0,1), 4)
                                            ,((0,1,0), 3)
                                            ,((1,2,1), 1)
                                            ]

  describe "Laws" $ do
    describe "1D-Bool"  $ do
      monoid (comparesEq :: Comparator (RTree Int Bool))
      traversable (cast :: Cast (RTree Int Bool))
      describe "counter-example" $ do
        let a = Leaf (1,4) False
            b = Region (-5,6) (RT.sortKids (Leaf (3,6) False :| [Leaf (-5,3) True,Leaf (0,1) True]))
            c = Region (3,10) (RT.sortKids (Leaf (4,7) False :| [Leaf (5,10) False,Leaf (3,6) True]))
            eq = comparesEq :: Comparator (RTree Int Bool)
            l_assoc = (a <> b) <> c
            r_assoc = a <> (b <> c)
        it "has same size" $ size l_assoc === size r_assoc
        it "comparesEq" $ (l_assoc `eq` r_assoc)
    describe "Dim3Set"  $ do
      monoid (comparesEq :: Comparator Dim3Set)
      traversable (cast :: Cast Dim3Set)
    describe "2D Chars" $ do
      monoid (comparesEq :: Comparator (RTree (Int,Int) Char))
      traversable (cast :: Cast (RTree (Int,Int) Char))
    describe "4D Word"  $ do
      monoid (comparesEq :: Comparator (RTree (Int,Int,Int,Int) Word))
      traversable (cast :: Cast (RTree (Int,Int,Int,Int) Word))

pair :: (Int,Int) -> (Int,Int)
pair = id

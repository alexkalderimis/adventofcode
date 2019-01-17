module Elves.RTreeSpec (spec) where

import           Control.Lens       hiding (index)
import qualified Data.Ix            as Ix
import qualified Data.List          as L
import qualified Data.List.NonEmpty as NE
import           Test.Hspec
import           Test.QuickCheck    hiding (within)
import qualified Test.QuickCheck    as QC

import           Elves.Coord
import           Elves.RTree

type Dim3 = (Int,Int,Int)
type Dim3Set = RTree (Int,Int,Int) ()

newtype Unique a = Unique [a] deriving (Show)

instance (Arbitrary a, Ord a) => Arbitrary (Unique a) where
  arbitrary = Unique . L.nub . L.sort <$> arbitrary

newtype Cube = Cube { getCube :: ((Int,Int,Int), (Int,Int,Int)) } deriving (Show, Eq)

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

makeCloser :: Int -> Int -> (Int,Int)
makeCloser a b = case b - a of
  0 -> (a,b)
  1 -> (a + 1, b)
  _ -> (a + 1, b - 1)

query1 :: Dim3 -> Dim3Set -> [(Dim3,())]
query1 i t = take 1 $ query (i,i) t

subregions :: RTree i a -> [RTree i a]
subregions (Region _ ts) = NE.toList ts
subregions _             = []

maxRegionSize :: RTree i a -> Int
maxRegionSize t = let rs = subregions t
                   in maximum (length rs : fmap maxRegionSize rs)

tree :: [Dim3] -> RTree Dim3 ()
tree = foldr (\p -> insert p ()) Tip

spec :: Spec
spec = describe "Elves.RTree" $ do
  describe "size" $ do
    it "always has the size of the elements you put in it" $ property $ \elems ->
      let t = index elems
       in size (t :: Dim3Set) == length elems

  describe "query" $ do
    it "cannot find what has not been inserted" $ property $ \(NonEmpty (e:elems)) ->
      let t = index (filter (/= e) elems)
       in query1 (fst e) t == []
    it "always finds what has been inserted" $ property $ \(NonEmpty (e:elems)) ->
      let t = index (filter (/= e) elems)
       in query1 (fst e) (insert (fst e) (snd e) t) == [e]
    it "always finds what has been indexed" $ property $ \(NonEmpty (e:elems)) ->
      let t = index (e : elems)
       in query1 (fst e) t == [e]
    it "expanding a query never makes it less specific" $ property $ \(NonEmpty (e:elems)) ->
      let t = index (e : elems)
       in (e :: (Dim3, ())) `elem` query (expandQuery 3 $ (fst e, fst e)) t
    it "can find the midpoint in this line" $ do
      let t = Region ((0,-4,0,0),(0,4,0,0)) (NE.fromList [Leaf (0,0,0,0) ()
                                                         ,Leaf (0,4,0,0) ()
                                                         ,Leaf (0,-4,0,0) ()
                                                         ])
      query ((-3,-3,-3,-3),(3,3,3,3)) t `shouldSatisfy` elem ((0,0,0,0), ())


  describe "within" $ do
    specify "all cubes that are within other cubes also overlap" $ property $ \(Cube a) (Cube b) ->
      if (a `within` b) then overlaps a b else True
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
      $ property $ \a b ->
        a == b || nearestNeighbour manhattan a (tree [a,b]) == Just (b,())
    specify "in any tree, no point is closer than the NN"
      $ property $ \(Unique points) -> case points of
        (pnt:x:rst) -> let mnn = nearestNeighbour manhattan pnt (tree points)
                        in maybe False ((manhattan pnt x >=) . manhattan pnt . fst) mnn
        _ -> True

    -- manhattan-distance is very likely to lead to collisions, so
    -- just verify the distance.
    specify "nearestNeighbour and nearestNeighbour2 are equivalent, manhattan-wise"
      $ property $ \pnt (Unique points) ->
        let t = tree points
            a = nearestNeighbour manhattan pnt t
            b = nearestNeighbour2 manhattan pnt t
            dist = manhattan pnt . fst
         in fmap dist a == fmap dist b
    -- straight-line distance is unlikely to have random collisions.
    specify "nearestNeighbour and nearestNeighbour2 are equivalent, straight-line"
      $ property $ \pnt (Unique points) ->
        let t = tree points
            a = nearestNeighbour straightLine pnt t
            b = nearestNeighbour2 straightLine pnt t
         in a == b

  describe "nearestNeighbourK" $ do
    specify "it returns values in ascending order" $ property $ \p points ->
      let t = tree points
          f = straightLine
          matches = f p . fst <$> nearestNeighbourK f 5 p t
       in and [ a <= b | (a,b) <- zip matches (tail matches) ]

  describe "insert" $ do
    it "increases size by one" $ property $ \t i ->
      size t + 1 == size (insert (i :: Dim3) () t)
    specify "insertion means queries are successful" $ property $ \t i ->
      query1 i (insert i () t) == [(i :: Dim3,())]
    describe "A tree with sub regions" $ do
      let t = Region (1,10) $ NE.fromList [ Region (1,3)  $ NE.fromList [Leaf 1 (), Leaf 3 ()]
                                          , Region (8,10) $ NE.fromList [Leaf 8 (), Leaf 10 ()]
                                          ]
      it "does not add a new direct child if it is contained by a sub-region" $ do
        let t' = insert (2 :: Int) () t
        length (subregions t') `shouldBe` 2
      it "does add a new direct child if it is not contained by a sub-region" $ do
        let t' = insert (5 :: Int) () t
        length (subregions t') `shouldBe` 3
  describe "maxPageSize" $ do
    let maxRegionSize t = case t of Region _ ts -> maximum (NE.cons (length ts) (fmap maxRegionSize ts))
                                    _ -> 0

    specify "after indexing, no region is larger than the max-page-size" $ property $ \t ->
      maxRegionSize (t :: Dim3Set) <= maxPageSize
    specify "after inserting, no region is larger than the max-page-size" $ property $ \(NonEmpty elems) ->
      let t = foldr (\i -> insert (i :: Dim3) ()) Tip elems
       in maxRegionSize t <= maxPageSize


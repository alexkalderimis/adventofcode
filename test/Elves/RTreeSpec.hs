{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}

module Elves.RTreeSpec (spec) where

import           Prelude                  hiding (lookup)

import           Control.Arrow            (first)
import           Control.Category         ((>>>))
import           Control.Concurrent       (threadDelay)
import qualified Control.Concurrent.Async as Async
import           Control.Exception.Base   (Exception)
import           Control.Lens             hiding (index, equality)
import           Control.Monad.Trans.Writer.CPS (Writer, runWriter, tell)
import qualified Data.Foldable            as F
import           Data.Functor.Compose
import qualified Data.Ix                  as Ix
import qualified Data.List                as L
import           Data.List.NonEmpty       (NonEmpty (..))
import qualified Data.List.NonEmpty       as NE
import           Data.Semigroup           (sconcat)
import           Test.Hspec
import           Test.QuickCheck          hiding (within)
import qualified Test.QuickCheck          as QC

import           Elves
import           Elves.Coord hiding (size, extent)
import           Elves.LawsSpec
import           Elves.RTree              hiding (null)
import qualified Elves.RTree              as RT

import Support.BoundingBoxes (Dim2, Dim3, Cube(..), Range(..))
import Support.RTreeSupport

spec :: Spec
spec = describe "Elves.RTree" $ do
  sizeSpec
  querySpec
  lookupSpec
  expandQuerySpec
  nearestNeighbourSpec
  nearestNeighbourKSpec
  insertSpec
  maxPageSizeSpec
  nullSpec
  deleteSpec
  alterSpec
  sizeWithSpec
  insertWithSpec
  stackOfCardsSpec
  lawsSpec
  decontructionSpec

decontructionSpec = describe "deconstruct" $ do
  describe "leaves" $ do
    specify "mconcat (leaves t) === t" $ property $ \t ->
      mconcat (leaves t) `eq` (t :: RTree Dim3 Word)
  describe "subtrees" $ do
    specify "sconcat (subtrees t) === t" $ property $ \t ->
      sconcat (subtrees t) `eq` (t :: RTree Dim3 Word)

sizeSpec = describe "size"
  $ it "always has the size of the elements you put in it"
  $ property $ \(Unique elems) ->
    let t = fromPoints elems in size (t :: Dim3Set) == length elems

querySpec = describe "query" $ do
    it "no-false-positives" $ property $ \e elems ->
      let t = tree $ filter (/= e) elems
       in query1 e t === []
    it "no-false-negatives-insertPoint" $ property $ \e elems ->
      let t = tree elems
       in query1 e (insertPoint e () t) === [(e,())]
    it "no-false-negatives" $ property $ \e elems ->
      let t = fromPoints (e : elems)
       in query1 (fst e) t === [e]
    describe "any strategy" $ do
      it "no-false-negatives" $ property $ \strategy e elems ->
        let t = RT.fromList [(getCube x, ()) | x <- e : elems ]
         in (getCube e, ()) `elem` query strategy (getCube e) t

    describe "QueryStrategy" $ do
      --- -----0+++++++++1+++++++++2+++++++++3
      --  5----0----5----0----5----0----5----0
      --       [=========]                     A
      --            [=========]                B
      --            [====]                     C
      --  [===]                                E
      --                           [=========] D
      --                              [====]   F
      let k = (,) :: Int -> Int -> (Int, Int)
          a = singleton (k 0 10) 'A' :: RTree Int Char
          b = singleton (k 5 15) 'B'
          c = singleton (k 5 10) 'C'
          d = singleton (k 20 30) 'D'
          e = singleton (k (-5) (-1)) 'E'
          f = singleton (k 23 27) 'F'
          t = mconcat [a,b,c,d,e,f]
      let search i s      = query s i t
          shouldFind x    = (`shouldContain` elems x) . fmap snd
          shouldNotFind x = (`shouldNotContain` elems x) . fmap snd

      consider Precisely $ do
        forM_ [a,b,c,d,e,f] $ \t -> do
          let i = bounds' t
              a = head (elems t)
          which (show i <> " matches only itself") (search i >>> (`shouldBe` [(i,a)]))

      consider Within $ do
        forM_ [a,b,c,d,e,f] $ \t -> do
          let i = bounds' t
          which (show i <> " matches at least itself") (search i >>> shouldFind t)
        which "finds f within d"         (search (20,30) >>> shouldFind f)
        which "does not find a within d" (search (20,30) >>> shouldNotFind a)
        which "finds c within a"         (search ( 0,10) >>> shouldFind c)
        which "does not find b within a" (search ( 0,10) >>> shouldNotFind b)
        which "does not find a within b" (search ( 5,15) >>> shouldNotFind a)
        which "finds b within (0, 20)"   (search ( 0,20) >>> shouldFind b)
        which "does not find d inside a" (search ( 0,10) >>> shouldNotFind d)

      consider Overlapping $ do
        forM_ [a,b,c,d,e,f] $ \t -> do
          let i = bounds' t
          which (show i <> " matches at least itself") (search i >>> shouldFind t)
        forM_ (pairs [a,b,c]) $ \(x, y) -> do
          let k = bounds' x
          which ("finds " <> elems y <> " overlapping " <> elems x) (search k >>> shouldFind y)
        forM_ (pairs [d, f]) $ \(x, y) -> do
          let k = bounds' x
          which ("finds " <> elems y <> " overlapping " <> elems x) (search k >>> shouldFind y)
        forM_ [a,b,c,d,f] $ \t -> do
          let i = bounds' t
          which ("does not find E overlapping " <> elems t) (search i >>> shouldNotFind e)
          which ("does not find " <> elems t <> " E") (search (-5, -1) >>> shouldNotFind t)

    it "expanding a query never makes it less specific" $ property $ \e elems ->
      let t = fromPoints (e : elems)
       in (first dbl e) `elem` query Within (expandQuery 3 $ (fst e, fst e)) (t :: Dim3Set)
    it "can find the midpoint in this line" $ do
      let p k = singleton (dbl k) ()
          t = region $ NE.fromList [p (0,0,0,0), p (0,4,0,0), p (0,-4,0,0)]
      query Within ((-3,-3,-3,-3),(3,3,3,3)) t `shouldSatisfy` elem (dbl (0,0,0,0), ())

lookupSpec = describe "lookup" $ do
  let oneDB = (cast :: Cast (RTree Int Bool))

  specify "We can find anything present in the tree" $ property $ \(Cube bs) x t ->
    lookup bs (singleton bs x <> t) === Just (x :: Word)

  specify "We cannot find anything not present in the tree"
   $ property $ \(Cube bs) xs ->
    let t = RT.fromList (filter ((/= bs) . fst) xs)
     in lookup bs t === (Nothing :: Maybe Word)

  specify "lookup q t === L.lookup q (assocs t)" $ property $ \(ValidBounds q) t ->
    lookup q (oneDB t) === L.lookup q (assocs t)

  specify "lookup q (a <> b) == if q `member` a then lookup q a else lookup q b"
    $ property $ \(ValidBounds q) a b ->
        let r = lookup q (a <> b)
         in if RT.member q (oneDB a)
               then r === lookup q a
               else r === lookup q b

expandQuerySpec = describe "expandQuery" $ do
    it "always includes the query" $ property $ \(NonNegative n) q ->
      Ix.inRange (expandQuery n (q,q :: Dim3)) q

nearestNeighbourSpec = describe "nearestNeighbour" $ do
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

nearestNeighbourKSpec = describe "nearestNeighbourK" $ do
  let distToP h p = measure h p . closestPoint p
      comesBefore f a b = let fa = f a
                              fb = f b
                              q x = "(" ++ show x ++ ")"
                           in QC.counterexample (unwords [ show a, q fa
                                                         , ">"
                                                         , show b, q fb
                                                         ])
                                            (fa <= fb)
  specify "it returns values in ascending order"
    $ property $ \h (NonNegative k) p t ->
      let matches = fst <$> nearestNeighbourK (measure h) k p (t :: Dim3Set)
       in foldr (.&&.) (property True) $ zipWith (comesBefore (distToP h p))
                                                 matches (tail matches)

  specify "counter-example-1" $ do
    let h = Manhattan
        k = 4
        p = (-3,0,-2)
        t = RT.fromList [(((-7,5,1),(0,12,14)), 'A')
                        ,(((-5,-1,-7),(-5,-1,-7)),'B')
                        ]
        [a,b] = fst <$> nearestNeighbourK (measure h) k p t
    comesBefore (distToP h p) a b

insertSpec = describe "insert" $ do

    describe "duplicate entry" $ do
      let dup = singleton (2,3)
          a = region $ NE.fromList [singleton ( 0,1) True, dup True]
          b = region $ NE.fromList [singleton (-1,2) False, dup False]
      it "does not exist" $ do
        size (a <> b :: RTree Int Bool) `shouldBe` 3
      it "has the value of the LHS when the LHS is a" $ do
        lookup (2,3) (a <> b) `shouldBe` lookup (2,3) a
      it "has the value of the LHS when the LHS is b" $ do
        lookup (2,3) (b <> a) `shouldBe` lookup (2,3) b
      describe "deeply nested entries" $ do
        let d1_tree = RT.fromList . flip zip (repeat ()) . fmap pair
            t1 = d1_tree [(-15,-10),(0,5),(4,7),( 9,18),(10,20),(15,16),(17,25)]
            t2 = d1_tree [(-15, -9),(1,6),(5,8),(10,19),(11,21),(15,16),(18,26)]
        specify "<> acts like set-union" $ do
          size (t1 <> t2) === (size t1) + (size t2) - 1

    describe "counter-example-1" $ do
      let a = region $ NE.fromList [ singleton (-2,1) False
                                   , singleton (3,3) True
                                   ]
          b = region $ NE.fromList [ singleton (0,3) True
                                   , singleton (0,4) False
                                   ]

      specify "We can combine these regions" $ QC.within 100 $ do
        size (a <> b :: RTree Int Bool) === 4

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
      let a = singleton (( 0,  0), ( 4,  8)) 'A'
          b = singleton (( 0, 10), ( 4, 20)) 'B'
          c = singleton (( 5,  0), ( 9, 11)) 'C'
          d = singleton (( 5, 13), ( 9, 20)) 'D'
          e = singleton (( 2,  6), ( 8, 16)) 'E'
          f = singleton (( 3,  3), ( 6, 17)) 'F'
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

    it "increases-size" $ property $ \t i -> QC.within 100000 $ do
      let delta = maybe 1 (pure 0) (lookup (i,i) t)
          t' = insertPoint (i :: Dim3) () t
      size t + delta `shouldBe` size t'

    it "handles this other quick-check counterexample" $ do
      let objects = [(((-59,-49,-29),(-36,-36,-12)), 'A')
                    ,(((-55,-29,-31),(-14,-23,-16)), 'B')
                    ,(((-29,-51,-33),(-4,-49,3)),    'C')
                    ,(((-44,-16,-13),(40,18,25)),    'D')
                    ,(((-11,-56,-15),(-9,19,4)),     'E')
                    ,(((-10,-60,-20),(21,-13,-16)),  'F')
                    ,(((-58,3,23),(4,13,32)),        'G')
                    ,(((-48,-15,-9),(-17,-10,55)),   'H')
                    ,(((-36,-47,2),(-8,21,37)),      'I')
                    ,(((-54,4,-24),(15,9,54)),       'J')
                    ,(((-47,-3,14),(31,7,57)),       'K')
                    ,(((-42,-4,-1),(42,16,28)),      'L')
                    ,(((-47,-49,39),(59,1,44)),      'M')
                    ,(((-58,14,56),(-26,45,61)),     'N')
                    ,(((-56,26,34),(30,34,35)),      'O')
                    ,(((-29,15,24),(31,31,52)),      'P')
                    ,(((-37,-52,45),(56,72,49)),     'Q')
                    ,(((-5,-5,32),(26,46,45)),       'R')
                    ,(((0,31,47),(13,37,56)),        'S')
                    ,(((-58,-1,-35),(-23,35,38)),    'T')
                    ,(((-46,6,-47),(1,36,12)),       'U')
                    ,(((-42,0,-50),(-22,51,-36)),    'V')
                    ,(((-52,30,-45),(60,54,-27)),    'W')
                    ,(((-44,14,16),(24,64,38)),      'X')
                    ,(((-36,-30,-49),(8,51,8)),      'Y')
                    ,(((-50,32,60),(47,64,73)),      'Z')
                    ,(((-17,47,-3),(37,63,16)),      'a')
                    ,(((-38,27,8),(75,63,41)),       'b')
                    ,(((-4,21,40),(26,76,42)),       'c')
                    ,(((6,43,34),(14,44,35)),        'd')
                    ,(((4,3,13),(32,48,62)),         'e')
                    ,(((38,29,-47),(72,41,79)),      'f')
                    ,(((40,41,35),(65,68,37)),       'g')
                    ,(((-8,35,-59),(59,35,-3)),      'h')
                    ,(((-1,51,-41),(22,67,59)),      'i')
                    ,(((34,-21,-40),(52,54,-34)),    'j')
                    ,(((47,44,-40),(63,52,-26)),     'k')
                    ,(((48,34,-52),(71,82,-39)),     'l')
                    ,(((59,42,-51),(61,81,-46)),     'm')
                    ,(((53,43,54),(90,90,66)),       'n')
                    ,(((-32,-22,10),(54,18,40)),     'o')
                    ,(((-30,-3,26),(58,2,49)),       'p')
                    ,(((19,-39,35),(43,60,36)),      'q')
                    ,(((21,-19,28),(62,-1,30)),      'r')
                    ,(((36,-41,31),(59,-37,36)),     's')
                    ,(((60,-13,-15),(68,20,60)),     't')
                    ,(((-25,-54,-22),(46,-18,32)),   'u')
                    ,(((-12,-33,-47),(39,46,-29)),   'v')
                    ,(((7,-29,-35),(18,-23,-30)),    'w')
                    ,(((-1,-48,-11),(51,-42,22)),    'x')
                    ,(((49,-31,-17),(56,-10,5)),     'y')
                    ,(((58,-3,-28),(69,28,56)),      'z')
                    ,(((57,-57,42),(69,36,53)),      'α')
                    ,(((52,55,-32),(65,100,112)),    'β')
                    ,(((58,54,28),(90,95,55)),       'γ')
                    ]
      let t = RT.fromList objects
          p = origin

      length (query Precisely (p,p) t) `shouldBe` 0
      fmap snd (query Overlapping (p,p) t) `shouldMatchList` "DLY"
      size (insertPoint p 'θ' t) `shouldBe` length objects + 1

    specify "makes-queries-work" $ property $ \t i ->
      query1 i (insertPoint i () t) == [(i :: Dim3,())]

maxPageSizeSpec = describe "maxPageSize" $ do
  specify "after indexing, no region is larger than the max-page-size" $ property $ \t ->
    maxRegionSize (t :: Dim3Set) <= maxPageSize t
  specify "after inserting, no region is larger than the max-page-size" $ property $ \(NonEmpty elems) ->
    let t = foldr (\i -> insertPoint (i :: Dim3) ()) RT.empty elems
     in maxRegionSize t <= maxPageSize t

nullSpec = describe "null" $ do
    specify "null lists make null trees" $ property $ \ps ->
      RT.null (tree ps) == null ps

deleteSpec = describe "delete" $ do

  it "reduces tree size" $ property $ \p ps ->
    let t = tree (p:ps)
     in size t > size (delete (dbl p) t)

  it "makes points impossible to find" $ property $ \p ps ->
    null (query1 p . delete (dbl p) $ tree (p:ps))

  it "ensures that member returns False" $ property $ \p ps ->
    let t = tree (p:ps)
        q = dbl p
     in member q (delete q t) === False

alterSpec = describe "alter" $ do
  let points = [0..10] <> [100..500]
      n = length points
      t = RT.fromPoints $ zip points (repeat 1) :: RTree Int Int
      f ma = case ma of Nothing -> Just 1
                        Just n -> if n == 3 then Nothing else Just (n + 1)

  specify "it can insert" $ do
    let t' = alter f (12,15) t
    size t' `shouldBe` n + 1
    lookup (12,15) t' `shouldBe` Just 1
    lookup (12,15) t `shouldBe` Nothing

  specify "it can modify" $ do
    let t' = alter f (2,2) t
    size t' `shouldBe` n
    lookup (2,2) t' `shouldBe` Just 2
    lookup (2,2) t `shouldBe` Just 1

  specify "it can insert, modify and delete" $ do
    let ts = take 5 $ iterate (alter f (2,2)) t
    fmap size ts `shouldBe` [n, n, n, n - 1, n]
    fmap (lookup (2,2)) ts `shouldBe` [Just 1, Just 2, Just 3, Nothing, Just 1]

  describe "calling alterM in a monadic context" $ do
    let f mv = do tell [mv]
                  case mv of Nothing -> pure (Just 1)
                             Just n -> pure (Just (n + 1))

    specify "it calls the function no more than once" $
      property $ \points (Range p) -> do
        let t = RT.fromListWith (+) [ (getRange p, 1 :: Int) | p <- points ]
            (t', values) = runWriter (RT.alterM f p t)
            prev = RT.lookup p t
            curr = fromMaybe 0 $ RT.lookup p t'

        curr `shouldBe` fromMaybe 0 prev + 1
        values `shouldBe` [prev]

  specify "it handles multiple including regions/leaves, 1D" $ do
    let p = (5, 10) :: Bounds Int
        t = RT.fromList [ ((0, 10), "abc")
                        , (p,       "def")
                        , ((5, 20), "ghi")
                        ]
    elems (alter (fmap tail) p t) `shouldMatchList` ["abc", "ef", "ghi"]

  specify "it handles multiple including regions/leaves, 2D" $ do
    let p = ((5, 0), (10, 0)) :: Bounds Dim2
        t = RT.fromList [ (((0, 0), (10, 0)), "abc")
                        , (p, "def")
                        , (((5, 0), (20, 0)), "ghi")
                        ]
    elems (alter (fmap tail) p t) `shouldMatchList` ["abc", "ef", "ghi"]

sizeWithSpec = describe "sizeWith" $ do
    specify "is always extent t when adding a Tip" $ property $ \t ->
      sizeWith t RT.empty == extent (t :: Dim3Set)
    specify "is always >= extent t when adding a tree" $ property $ \t0 t1 ->
      sizeWith t0 t1 >= extent (t0 :: Dim3Set)
    specify "can calculate the new size" $ do
      sizeWith (singleton (dbl (9,9,9)) ()) (tree [(0,0,0),(3,1,5)]) `shouldBe` 1000

insertWithSpec = describe "insertWith" $ do
  let trues = fromPoints $ flip zip (repeat True) (Ix.range ((0,0), (10,10)))

  specify "we can choose old values" $ do
    let bs = dbl (5,5)
        t = insertWith (\new old -> old) bs False trues

    lookup bs t `shouldBe` Just True

  specify "we can choose new values" $ do
    let bs = dbl (5,5)
        t = insertWith (\new old -> new) bs False trues

    lookup bs t `shouldBe` Just False

  specify "it can operate as counting structure" $ property $ do
    let t = fromPointsWith (+) $ flip zip (repeat 1)
                 [(0,0,0),(0,0,1),(0,1,0),(1,2,1)
                 ,(0,0,0),(0,0,1),(0,1,0)
                         ,(0,0,1),(0,1,0)
                         ,(0,0,1)
                 ]
        result = first fst <$> assocs t

    result `shouldMatchList` [((0,0,0), 2)
                             ,((0,0,1), 4)
                             ,((0,1,0), 3)
                             ,((1,2,1), 1)
                             ]

stackOfCardsSpec = describe "stack-of-cards" $ do
    -- test that we can use an RTree where every object overlaps with every
    -- other object. As an example, imagine a skewed stack of cards:
    --
    --  +---+
    --  | +---+
    --  | | +---+
    --  | | |   |
    --  +-| |   |
    --    +-|   |
    --      +---+
    -- In this case every card overlaps every other card, but they do not cover
    -- all the same points.
    let mkCard x y a = ( ((x,y),(x + (20 :: Int), y + (100 :: Int))), a )
        cards = zipWith (\p chr -> mkCard p p chr) [1 .. 20] ['a' ..]
        tests t = do
          let limit = 100 * 4000
              withCard t = QC.within limit $ QC.forAll (QC.elements cards) t
          it "can used to build a tree" $ QC.within limit $ do
            size t `shouldBe` length cards
          it "can select a known card" . withCard $ \card ->
              RT.lookup (fst card) t === Just (snd card)
          it "can overwrite a specific card" . withCard $ \card ->
              let t' = insertWith pure (fst card) 'X' t
               in size t' === size t .&&. t =/= t'
          it "an overwritten card stores the correct value" . withCard $ \card ->
              let t' = insertWith pure (fst card) 'X' t
               in RT.lookup (fst card) t' === Just 'X'
          it "can find the cards beneath a card" . withCard $ \card ->
              let origin = fst . fst $ card
                  r = snd <$> RT.query Overlapping (origin, origin) t
               in r === ['a' .. snd card]
          it "knows all cards overlap each other" . withCard $ \card ->
              let r = snd <$> RT.query Overlapping (fst card) t
               in r === fmap snd cards
          it "can insert any additional card" 
            $ let origins = fst . fst <$> cards
                  newOrigin = arbitrary `suchThat` (`notElem` origins)
               in QC.within limit $
                  QC.forAll newOrigin $ \(x,y) ->
                    let card = mkCard x y 'X'
                        t' = insertWith pure (fst card) (snd card) t
                     in size t' === (size t + 1)

    describe "fromList" (tests $ RT.fromList cards)
    describe "mconcat" (tests $ mconcat [singleton bs a | (bs,a) <- cards])

lawsSpec = describe "Laws" $ do
  oneDBools
  twoDChars
  dim3Sets
  fourDWords

oneDBools = describe "1D-Bool"  $ do
  let oneDB = (cast :: Cast (RTree Int Bool))
  monoid (eq :: Comparator (RTree Int Bool))
  traversable oneDB
  equality oneDB (lookup (10, 17))
           (mconcat . reverse . leaves)
           (\t -> if null t
                     then insert (10,17) True t
                     else mconcat . drop 1 $ leaves t)
    
  oneDBoolCounterExamples

oneDBoolCounterExamples = do
  let oneDB = (cast :: Cast (RTree Int Bool))
      is = eq :: Comparator (RTree Int Bool)
      t_o = 20000
      sgCounter name a b c = describe name $ do
        let l_assoc = (a <> b) <> c
            r_assoc = a <> (b <> c)
        -- runIO $ do
        --   putStrLn name
        --   putStrLn "A:"
        --   putStrLn (drawTree a)
        --   putStrLn "B:"
        --   putStrLn (drawTree b)
        --   putStrLn "C:"
        --   putStrLn (drawTree c)

        it "has same size" $ QC.within t_o (size l_assoc === size r_assoc)
        it "comparesEq"    $ QC.within t_o (l_assoc `is` r_assoc)

  sgCounter "counter-example-1"
     (RT.fromList [((1,4), False)])
     (RT.fromList [((3,6), False), ((-5,3), True), ((0,1), True)])
     (RT.fromList [((4,7), False), ((5,10), False), ((3,6), True)])

  describe "counter-example-2" $ do
    let t = RT.fromList [((-62,-19),False)
                        ,(( 39,47),False)
                        ,(( 53,77),True)
                        ,(( 61,68),True)
                        ,(( 67,74),False)
                        ]
    it "has the correct size" $ QC.within 1000 $ do
      size t == 5
    it "does not change when adding mempty" $ QC.within 1000 $ do
      (mempty <> t) `is` t
    it "does not change when adding to mempty" $ QC.within 1000 $ do
      (t <> mempty) `is` t

  sgCounter "counter-example-3"
      (RT.fromList [((-1,1),True)])
      (RT.fromList [((-1,3),False)])
      (RT.fromList [((-1,0),False),((-1,0),False),((2,3),True)])

  sgCounter "counter-example-4"
      (RT.fromList [((5,8),False)])
      (RT.fromList [((4,5),True),((6,9),False)])
      (RT.fromList [((4,9),False),((5,8),True)])

  sgCounter "counter-example-5"
      (RT.fromList [((-1, 9),True)])
      (RT.fromList [])
      (RT.fromList [((-5, 2),False)
                   ,(( 0, 7),False)
                   ,(( 8,13),True)
                   ,((10,10),False)
                   ,(( 9,13),False)
                   ])

  sgCounter "counter-example-6"
      (RT.fromList [((-3,-3),True),((-3,4),True)])
      (RT.fromList [])
      (RT.fromList [((-4,-2),True),((0,3),True),((1,5),True),((2,3),True)])

  sgCounter "counter-example-7"
    (RT.fromList [((6,9),True)])
    (RT.fromList [((-12,3),False)
                 ,((-3,-1),True)
                 ,((-1,1),False)
                 ,((1,2),True)
                 ,((1,5),False)
                 ,((1,8),False)
                 ,((4,4),False)
                 ,((4,5),True)
                 ,((4,11),True)
                 ,((6,9),False)
                 ,((9,17),True)
                 ])
    (RT.fromList [((10,18),True)])

  sgCounter "counter-example-8"
    (RT.fromList [ ((17,29),False), ((20,21),False), ((24,27),True) ])
    (RT.fromList [ ((19,25),False), ((22,33),True),  ((24,27),False) ])
    (RT.fromList [((25,31),True)])

  sgCounter "counter-example-9"
    (RT.fromList [ ((-11,2),False)])
    (RT.fromList [ ((8,8),False)])
    (RT.fromList [ ((1,8),True)
                 , ((1,11),False)
                 , ((8,8),False)
                 , ((8,9),True)
                 , ((5,15),False)
                 ])

  -- spurious counter-example. Reported as failure from quick-check but
  -- actually OK
  sgCounter "counter-example-10"
    (RT.fromList [((2,79),False),((10,30),False)])
    (RT.fromList [((7,77),False),((26,63),True)])
    (RT.fromList [ ((-77,17),False)
                 , ((-75,42),False) , ((-75,51),True) , ((-74,73),False)
                 , ((-55,62),True) , ((-50,77),False) , ((-43,71),True)
                 , ((-33,58),True) , ((-25,45),True) , ((-21,68),False)
                 , ((-17,51),True) , ((-71,41),True) , ((-70,32),True)
                 , ((-74,23),False) , ((-69,28),True) , ((-68,22),False)
                 , ((-74,17),False) , ((-60,-35),True) , ((-53,-1),False)
                 , ((-52,-17),False) , ((-41,22),False) , ((-36,-5),False)
                 , ((-40,-17),True) , ((-39,-16),True) , ((-28,-1),True)
                 , ((-32,-8),True) , ((-32,-2),False) , ((-31,-17),True)
                 , ((-22,-3),False) , ((-74,-40),True) , ((-62,-52),False)
                 , ((-56,-50),True),((-5,65),True),((-4,68),True),((-1,9),True)
                 , ((-1,6),False)
                 , ((3,70),False),((0,30),True),((2,8),True),((2,55),False)
                 , ((3,32),False),((6,32),False),((8,36),True),((10,30),True)
                 , ((9,27),False),((11,50),False),((14,21),False),((3,58),True)
                 , ((23,43),True),((23,71),False),((24,75),False),((29,50),True)
                 , ((28,74),True),((34,77),True),((47,85),True),((58,77),False)
                 , ((43,74),False),((52,74),False),((37,73),True),((42,73),True)
                 , ((31,35),True),((38,64),False),((34,62),True),((49,69),False)
                 , ((51,63),False),((59,60),False),((36,39),False),((36,60),False)
                 , ((39,47),True),((42,60),True),((59,78),False),((59,86),False)
                 , ((62,79),False),((67,82),True),((71,75),False),((72,89),False)
                 , ((74,77),False),((73,73),False)
                 ])

dim3Sets = describe "Dim3Set"  $ do
  monoid (eq :: Comparator Dim3Set)
  traversable (cast :: Cast Dim3Set)
  equality (cast :: Cast Dim3Set) (lookup ((0,0,0), (0,0,0)))
           (mconcat . reverse . leaves)
           (\t -> if null t
                     then insert ((0,0,0), (0,0,0)) () t
                     else mconcat . drop 1 $ leaves t)

twoDChars = describe "2D Chars" $ do
  monoid (eq :: Comparator (RTree (Int,Int) Char))
  traversable (cast :: Cast (RTree (Int,Int) Char))

fourDWords = describe "4D Word"  $ do
  monoid (eq :: Comparator (RTree (Int,Int,Int,Int) Word))
  traversable (cast :: Cast (RTree (Int,Int,Int,Int) Word))

pair :: (Int,Int) -> (Int,Int)
pair = id

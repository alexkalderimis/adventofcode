{-# LANGUAGE FlexibleContexts #-}

module Support.RTreeSupport where

import qualified Data.Array as A
import           Data.Array ((//))
import qualified Data.List.Extra          as L
import qualified Data.Tree                as T
import qualified Data.Foldable            as F
import           Data.Maybe
import           Control.Arrow            (first, second)
import qualified Data.List.NonEmpty as NE

import           Elves (applyN, Expectation, expectationFailure)
import           Elves.RTree              hiding (null)
import qualified Elves.RTree              as RT
import qualified Elves.StrictGrid as G
import Elves.Cartesian (up, down, left, right)

import           Test.QuickCheck          hiding (within)
import qualified Test.QuickCheck          as QC

import Support.BoundingBoxes (Dim2, Dim3, Cube(..))
import Support.TilingSupport (Depicted, coord, coords)
import qualified Support.TilingSupport as TS

newtype ValidBounds = ValidBounds
  { getValidBounds :: (Int,Int)
  } deriving (Show, Eq)

instance Arbitrary ValidBounds where
  arbitrary = do
    lb <- chooseInt (-500, 500)
    ub <- chooseInt (lb, 500)

    return (ValidBounds (lb,ub))

type Dim3Set = RTree Dim3 ()
type RangeTree a = RTree Int a

newtype Unique a = Unique { getUnique :: [a] } deriving (Show)
newtype UniqueKeys k v = UniqueKeys { getUniqueKeys :: [(k, v)] } deriving (Show)

instance (Arbitrary a, Ord a) => Arbitrary (Unique a) where
  arbitrary = Unique . L.nubOrd <$> arbitrary

instance (Arbitrary k, Arbitrary a, Ord k) => Arbitrary (UniqueKeys k a) where
  arbitrary = UniqueKeys . fmap head . L.groupOn fst . L.sortOn fst <$> arbitrary

data NNInput a = NNInput a a [a] deriving Show

instance (Arbitrary a, Ord a) => Arbitrary (NNInput a) where
  arbitrary = do
    Unique xs <- arbitrary
    x <- arbitrary `suchThat` (not . (`elem` xs))
    y <- arbitrary `suchThat` (not . (`elem` (x:xs)))
    return (NNInput x y xs)
  shrink (NNInput a b cs) = NNInput a b <$> shrink cs

shouldBeMemberOf :: (Show i, Show a, Queryable i) => Bounds i -> RTree i a -> Expectation
k `shouldBeMemberOf` t = if RT.member k t
                            then pure ()
                            else expectationFailure (show k <> " is not a member of " <> show t)

beMemberOf :: (Show i, Show a, Queryable i) => Bounds i -> RTree i a -> Expectation
beMemberOf = shouldBeMemberOf

treeProperty :: Testable prop => (RTree Dim3 Word -> prop) -> Property
treeProperty = property

query1 :: Dim3 -> Dim3Set -> [(Dim3,())]
query1 i = fmap (first fst) . take 1 . query Within (i,i)

maxRegionSize :: RTree i a -> Int
maxRegionSize t = let f = \_ sizes -> maximum (length sizes : sizes)
                   in maximum (0 : fmap (T.foldTree f) (RT.forest t))

tree :: [Dim3] -> Dim3Set
tree = RT.fromPoints . flip zip (repeat ())

dbl :: a -> (a,a)
dbl = (,) <$> id <*> id

-- not bad:
-- TODO: needs to be expanded (*1) to draw regions _outside_ the items they contain
depict :: RTree Dim2 Char -> Depicted
depict t = depictPoints
         . TS.depictOverlays dotted regions
         . TS.depictOverlays TS.intersectionBox zones
         . TS.depictOverlays TS.mainBox [bs]
         $ base 
  where
    bs = maybe ((0,0), (0,0)) (applyN (depth t) exterior . expandBs) (bounds t) :: (Dim2, Dim2)
    base = A.listArray (coords bs) (repeat ' ')
    zones = [ expandBs k | (k, _) <- assocs t, fst k /= snd k]
    depictPoints a = a // [(TS.coord (expand $ fst k), a) | (k, a) <- assocs t, fst k == snd k]
    regions = let go tree = let withd = NE.filter ((> 0) . fst)
                                      . fmap (\st -> (depth st, st))
                                      $ subtrees tree
                             in fmap regionBox withd ++ (fmap snd withd >>= go)
               in go t

    dotted = TS.Box Nothing '┊' '┄'
    expand (a,b) = let d = depth t * 2 in (a * d, b * d)
    expandBs = first expand . second expand
    regionBox (d,t) = applyN d exterior . expandBs $ bounds' t

exterior (lb, ub) = (up (left lb), down (right ub))

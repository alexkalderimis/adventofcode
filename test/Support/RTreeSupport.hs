module Support.RTreeSupport where

import qualified Data.List                as L
import qualified Data.Tree                as T
import           Control.Arrow            (first)

import           Elves.RTree              hiding (null)
import qualified Elves.RTree              as RT

import           Test.QuickCheck          hiding (within)
import qualified Test.QuickCheck          as QC

import Support.BoundingBoxes (Dim2, Dim3, Cube(..))

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

instance (Arbitrary a, Eq a) => Arbitrary (Unique a) where
  arbitrary = Unique . L.nub <$> arbitrary

data NNInput a = NNInput a a [a] deriving Show

instance (Arbitrary a, Ord a) => Arbitrary (NNInput a) where
  arbitrary = do
    Unique xs <- arbitrary
    x <- arbitrary `suchThat` (not . (`elem` xs))
    y <- arbitrary `suchThat` (not . (`elem` (x:xs)))
    return (NNInput x y xs)
  shrink (NNInput a b cs) = NNInput a b <$> shrink cs

query1 :: Dim3 -> Dim3Set -> [(Dim3,())]
query1 i = fmap (first fst) . take 1 . query Within (i,i)

maxRegionSize :: RTree i a -> Int
maxRegionSize t = let f = \_ sizes -> maximum (length sizes : sizes)
                   in maximum (0 : fmap (T.foldTree f) (RT.forest t))

tree :: [Dim3] -> Dim3Set
tree = RT.fromPoints . flip zip (repeat ())

depth :: RTree i a -> Int
depth = maximum . fmap (T.foldTree f) . RT.forest
  where
    f _ depths = 1 + maximum (0 : depths)

dbl :: a -> (a,a)
dbl = (,) <$> id <*> id

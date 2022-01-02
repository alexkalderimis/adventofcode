{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Support.BoundingBoxes (
  Dim2, Dim3,
  Range(..), Region(..), Cube(..), Tesseract(..),
  CubeWithPoint(..),
  CubeWithCube(..)
  )where

import Data.Coerce
import qualified Data.Ix as Ix
import Test.QuickCheck
import Control.Lens (ReifiedLens(..), ReifiedLens', lens, view, set, _1, _2, _3, _4)

type Dim2 = (Int,Int)
type Dim3 = (Int,Int,Int)
type Dim4 = (Int,Int,Int,Int)

-- one dimensional range
newtype Range = Range { getRange :: (Int, Int) } deriving (Show, Eq)

instance Arbitrary Range where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary `suchThat` (>= a)
    pure $ Range (a, b)

  shrink (Range r) = fmap Range . filter (/= r) $ [squeeze r (Lens $ lens id (\_ x -> x))]

newtype Region = Region { getRegion :: (Dim2, Dim2) } deriving (Show, Eq)

instance Arbitrary Region where
  arbitrary = do
    (a,b) <- arbitrary
    a' <- arbitrary `suchThat` (>= a)
    b' <- arbitrary `suchThat` (>= b)
    pure (Region ((a,b),(a',b')))

  shrink (Region r) = fmap Region .filter (/= r) . fmap (squeeze r) $ [Lens _1, Lens _2]

-- cube defined as range from lower-bound to upper-bound
newtype Cube = Cube { getCube :: (Dim3, Dim3) } deriving (Show, Eq)

cubeSize :: Cube -> Int
cubeSize = Ix.rangeSize . getCube

instance Arbitrary Cube where
  arbitrary = do
    (a,b,c) <- arbitrary
    a' <- arbitrary `suchThat` (>= a)
    b' <- arbitrary `suchThat` (>= b)
    c' <- arbitrary `suchThat` (>= c)
    pure (Cube ((a,b,c),(a',b',c')))

  shrink (Cube c) = fmap Cube
                  . filter (/= c)
                  . fmap (squeeze c)
                  $ [Lens _1, Lens _2, Lens _3]

newtype Tesseract = Tesseract { getTesseract :: (Dim4, Dim4) } deriving (Show, Eq)

instance Arbitrary Tesseract where
  arbitrary = do
    (a,b,c,d) <- arbitrary
    a' <- arbitrary `suchThat` (>= a)
    b' <- arbitrary `suchThat` (>= b)
    c' <- arbitrary `suchThat` (>= c)
    d' <- arbitrary `suchThat` (>= d)
    pure (Tesseract ((a,b,c,d),(a',b',c',d')))

  shrink (Tesseract c) = fmap Tesseract
                       . filter (/= c)
                       . fmap (squeeze c)
                       $ [Lens _1, Lens _2, Lens _3, Lens _4]

-- a cube along with one of the points within it
data CubeWithPoint = CubeWithPoint !Cube !Dim3 deriving (Show)

instance Arbitrary CubeWithPoint where
  arbitrary = do
    cube@(Cube ((a,b,c),(a',b',c'))) <- arbitrary
    x <- chooseInt (a, a')
    y <- chooseInt (b, b')
    z <- chooseInt (c, c')
    return (CubeWithPoint cube (x,y,z))

  shrink (CubeWithPoint c p) = [CubeWithPoint shrunk p | shrunk <- shrink c
                                                       , Ix.inRange (getCube shrunk) p
                               ]

-- a cube containing another cube within it
data CubeWithCube = CubeWithCube !Cube !Cube deriving (Show)

instance Arbitrary CubeWithCube where
  arbitrary = do
    (CubeWithPoint cube@(Cube (_,(a',b',c'))) (x,y,z)) <- arbitrary
    x' <- chooseInt (x,a')
    y' <- chooseInt (y,b')
    z' <- chooseInt (z,c')

    pure (CubeWithCube cube (Cube ((x,y,z),(x',y',z'))))

  shrink (CubeWithCube a b) = [CubeWithCube (Cube a') (Cube b') | (Cube a') <- shrink a
                                                                , (Cube b') <- shrink b
                                                                , Ix.inRange a' (fst b')
                                                                , Ix.inRange a' (snd b')
                              ]

makeCloser :: Int -> Int -> (Int,Int)
makeCloser a b = case b - a of
  0 -> (a,b)
  1 -> (a + 1, b)
  _ -> (a + 1, b - 1)

squeeze :: (a, a) -> ReifiedLens' a Int -> (a, a)
squeeze (lb, ub) d = (write a lb, write b ub)
  where
   read = view (runLens d)
   write = set (runLens d)
   (a,b) = makeCloser (read lb) (read ub)

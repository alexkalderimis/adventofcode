{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}

module Elves.Matrix.Rotate (
  Point,
  Rotation,
  pointX, pointY, pointZ, point,
  rotations, rotation,
  aroundX, aroundY, aroundZ,
  rotate_0,
  rotate_z_90, rotate_z_180, rotate_z_270,
  rotate_x_90, rotate_x_180, rotate_x_270,
  rotate_y_90, rotate_y_180, rotate_y_270,
  rotateBy
  ) where

import qualified Data.Array as A

import Elves.StrictGrid
import Elves.Matrix
import qualified Elves.Coord as C
import           Control.Lens    (Lens', ReifiedLens(..), lens)

import Test.QuickCheck.Arbitrary

-- points represented as a column vector
newtype Point a = P3 { columnVector :: Matrix a } deriving (Eq, Functor)

instance Arbitrary a => Arbitrary (Point a) where
  arbitrary = point <$> arbitrary <*> arbitrary <*> arbitrary

-- rotations represented as a square matrix
newtype Rotation a = R { unrot :: Matrix a } deriving (Eq, Show)

instance Show a => Show (Point a) where
  show (P3 m) = mconcat [ "{ "
                        , show (at 0 0 m)
                        , ", "
                        , show (at 1 0 m)
                        , ", "
                        , show (at 2 0 m)
                        , " }"
                        ]

instance Ord a => Ord (Point a) where
  compare (P3 a) (P3 b) = compare (at 0 0 a, at 1 0 a, at 2 0 a)
                                  (at 0 0 b, at 1 0 b, at 2 0 b)

instance Num a => Semigroup (Rotation a) where
  (R a) <> (R b) = R (dotp a b)

instance Num a => Monoid (Rotation a) where
  mempty = rotate_0

instance (Num a, Ord a) => C.Coord (Point a) where
  type Dimension (Point a) = a
  origin = point 0 0 0
  dimensions = [ Lens pointX, Lens pointY, Lens pointZ ]

pointX, pointY, pointZ :: Lens' (Point a) a
pointX = lens (\(P3 p) -> at 0 0 p) (\p x -> P3 (update 0 0 x (columnVector p)))
pointY = lens (\(P3 p) -> at 1 0 p) (\p x -> P3 (update 1 0 x (columnVector p)))
pointZ = lens (\(P3 p) -> at 2 0 p) (\p x -> P3 (update 2 0 x (columnVector p)))

point :: a -> a -> a -> Point a
point x y z = P3 (matrix (Row 3) (Col 1) [x, y, z])

-- rotations

rotations :: Num a => [Rotation a]
rotations = mconcat
  [ [ rx <> ry <> rotate_z_90  | ry <- aroundY, rx <- aroundX ]
  , [ rx <> rz                 | rz <- [rotate_0, rotate_z_180], rx <- aroundX ]
  ]
  where
    aroundX   = [rotate_0, rotate_x_90, rotate_x_180, rotate_x_270]
    aroundY   = [rotate_0, rotate_y_90, rotate_y_180, rotate_y_270]

-- around the Z axis

rotation :: (Num a, Floating a) => a -> a -> a -> Rotation a
rotation rz ry rx = R mrz <> R mry <> R mrx
  where mrz = matrix (Row 3) (Col 3) [ cos rz, negate (sin rz), 0
                                     , sin rz, cos rz,          0
                                     , 0     , 0              , 1
                                     ]
        mry = matrix (Row 3) (Col 3) [ cos ry         , 0, sin ry
                                     , 0              , 1,      0
                                     , negate (sin ry), 0, cos ry
                                     ]
        mrx = matrix (Row 3) (Col 3) [ 1, 0, 0
                                     , 0, cos rx, negate (sin rx)
                                     , 0, sin rx, cos rx
                                     ]

aroundZ :: (Eq a, Num a) => Rotation a -> Bool
aroundZ = aroundAxis $ \i -> case col i of
            Col 0 -> Coord (Row 2) (Col 0)
            Col 1 -> Coord (Row 2) (Col 1)
            Col 2 -> Coord (Row 2) (Col 2)
            Col 3 -> Coord (Row 1) (Col 2)
            Col 4 -> Coord (Row 0) (Col 2)

aroundX :: (Eq a, Num a) => Rotation a -> Bool
aroundX = aroundAxis $ \i -> case col i of
            Col 0 -> Coord (Row 2) (Col 0)
            Col 1 -> Coord (Row 1) (Col 0)
            Col 2 -> Coord (Row 0) (Col 0)
            Col 3 -> Coord (Row 0) (Col 1)
            Col 4 -> Coord (Row 0) (Col 2)

aroundY :: (Eq a, Num a) => Rotation a -> Bool
aroundY = aroundAxis $ \i -> case col i of
            Col 0 -> Coord (Row 0) (Col 1)
            Col 1 -> Coord (Row 1) (Col 0)
            Col 2 -> Coord (Row 1) (Col 1)
            Col 3 -> Coord (Row 1) (Col 2)
            Col 4 -> Coord (Row 2) (Col 1)

aroundAxis :: (Eq a, Num a) => (Coord -> Coord) -> Rotation a -> Bool
aroundAxis f r = a == b
  where
    a = getMatrix $ matrix (Row 1) (Col 5) [0, 0, 1, 0, 0]
    b = A.ixmap (A.bounds a) f (getMatrix $ unrot r)

rotate_0 :: Num a => Rotation a
rotate_0 = R (identity 3)

rotate_z_90 :: Num a => Rotation a
rotate_z_90 = R $ matrix (Row 3) (Col 3) [ 0, (-1), 0
                                         , 1,    0, 0
                                         , 0,    0, 1
                                         ]

rotate_z_180 :: Num a => Rotation a
rotate_z_180 = rotate_z_90 <> rotate_z_90

rotate_z_270 :: Num a => Rotation a
rotate_z_270 = rotate_z_90 <> rotate_z_180

-- around the X axis

rotate_x_90 :: Num a => Rotation a
rotate_x_90 = R $ matrix (Row 3) (Col 3) [ 1, 0,  0
                                     , 0, 0, -1
                                     , 0, 1,  0
                                     ]

rotate_x_180 :: Num a => Rotation a
rotate_x_180 = rotate_x_90 <> rotate_x_90

rotate_x_270 :: Num a => Rotation a
rotate_x_270 = rotate_x_90 <> rotate_x_180

-- around the Z axis

rotate_y_90 :: Num a => Rotation a
rotate_y_90 = R $ matrix (Row 3) (Col 3) [ 0,    0,    1
                                         , 0,    1,    0
                                         , -1,   0,    0
                                         ]

rotate_y_180 :: Num a => Rotation a
rotate_y_180 = rotate_y_90 <> rotate_y_90

rotate_y_270 :: Num a => Rotation a
rotate_y_270 = rotate_y_90 <> rotate_y_180

rotateBy :: Num a => Rotation a -> Point a -> Point a
rotateBy (R r) (P3 p) = P3 (r `dotp` p)

{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}

module Elves.Coord.Strict where

import Control.Lens.Combinators (Lens, view, ReifiedLens(..))
import qualified Data.Ix as Ix
import           Data.Ix (Ix)

import Elves.Coord

data Point2 = P2 { p2x :: {-# UNPACK #-} !Int
                 , p2y :: {-# UNPACK #-} !Int
                 } deriving (Eq, Show, Ord)

data Point3 = P3 { p3x :: {-# UNPACK #-} !Int
                 , p3y :: {-# UNPACK #-} !Int
                 , p3z :: {-# UNPACK #-} !Int
                 } deriving (Eq, Show, Ord)

newtype Row = Row { getRow :: Int } deriving (Show, Eq, Ord, Ix, Enum, Num)
newtype Col = Col { getCol :: Int } deriving (Show, Eq, Ord, Ix, Enum, Num)

data Coordinate = Coord 
  { row :: {-# UNPACK #-} !Row
  , col :: {-# UNPACK #-} !Col
  } deriving (Show, Eq, Ord)

class PointX a where
  _px :: Lens a a Int Int

class PointY a where
  _py :: Lens a a Int Int

class PointZ a where
  _pz :: Lens a a Int Int

instance PointX Int where
  _px f i = f i

instance PointX Point2 where
  _px f p = fmap (\i -> p { p2x = i }) (f $ p2x p)

instance PointY Point2 where
  _py f p = fmap (\i -> p { p2y = i }) (f $ p2y p)

instance PointX Point3 where
  _px f p = fmap (\i -> p { p3x = i }) (f $ p3x p)

instance PointY Point3 where
  _py f p = fmap (\i -> p { p3y = i }) (f $ p3y p)

instance PointZ Point3 where
  _pz f p = fmap (\i -> p { p3z = i }) (f $ p3z p)

instance PointX Coordinate where
  _px f c = fmap (\i -> c { col = Col i }) (f . getCol $ col c)

instance PointY Coordinate where
  _py f c = fmap (\i -> c { row = Row i }) (f . getRow $ row c)

px :: PointX a => a -> Int
px = view _px

py :: PointY a => a -> Int
py = view _py

pz :: PointZ a => a -> Int
pz = view _pz

instance Coord Point2 where
  type Dimension Point2 = Int
  origin = P2 0 0
  dimensions = [ Lens _px , Lens _py ]

instance Coord Point3 where
  type Dimension Point3 = Int
  origin = P3 0 0 0
  dimensions = [ Lens _px , Lens _py , Lens _pz ]

instance Coord Coordinate where
  type Dimension Coordinate = Int
  origin = Coord 0 0
  dimensions = [ Lens _py, Lens _px ]

instance Ix Coordinate where
  range (a, b) = [Coord r c | (r, c) <- Ix.range ((row a, col a), (row b, col b))]
  index (a, b) x = Ix.index ((row a, col a), (row b, col b)) (row x, col x)
  inRange (a, b) x = Ix.inRange ((row a, col a), (row b, col b)) (row x, col x)

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Elves.Matrix.RotateSpec (spec) where

import Data.Maybe
import Data.Coerce
import qualified Data.List as L
import Control.Arrow ((&&&))
import Test.Hspec
import Test.QuickCheck
import Control.Lens (over, view, set, ReifiedLens(..))
import Elves.Matrix.Rotate

import Elves.Coord (magnitude, origin)

newtype NonOrigin = NonOrigin { nonOrigin :: Point Int } deriving (Show, Eq)

type Cube = ( Point Int, Point Int, Point Int, Point Int
            , Point Int, Point Int, Point Int, Point Int
            )

-- a companion cube, for playing with
unitCube :: Cube
unitCube = 
    let  a = point 1 1 1
         b = flip pointX a
         c = flip pointY b
         d = flip pointX c
         [e, f, g, h] = fmap (flip pointZ) [a, b, c, d]
    in (a, b, c, d, e, f, g, h)
   where
     flip d = over d negate

instance Arbitrary NonOrigin where
  arbitrary = NonOrigin <$> (arbitrary `suchThat` (/= origin))

spec :: Spec
spec = describe "Elves.Matrix.Rotate" $ do
  let rzs = [rotate_0, rotate_z_90, rotate_z_180, rotate_z_270]
      rys = [rotate_0, rotate_y_90, rotate_y_180, rotate_y_270]
      rxs = [rotate_0, rotate_x_90, rotate_x_180, rotate_x_270]

  describe "rotations" $ do
    specify "there are 24 of them" $ do
      length (rotations :: [Rotation Int]) `shouldBe` 24

    it "contains all possible quarter turn rotations" $
      forAll (elements rzs) $ \rz ->
      forAll (elements rxs) $ \rx ->
      forAll (elements rys) $ \ry -> \(NonOrigin p) ->
        let p' = rotBy ry . rotBy rx $ rotBy rz p
         in L.find (\r -> rotBy r p == p') rotations `shouldSatisfy` isJust

    specify "all rotations are distinct" $
      withMaxSuccess 1000 $
      forAll (elements rotations)                    $ \r0 ->
      forAll (elements rotations `suchThat` (/= r0)) $ \r1 ->
        rcube r0 unitCube `shouldNotBe` rcube r1 unitCube

  describe "predicates" $ do
    it "knows all Z rotations are around Z axis" $
      forAll (elements rzs) $ \r -> r `shouldSatisfy` aroundZ
    it "knows all Y rotations are around Y axis" $
      forAll (elements rys) $ \r -> r `shouldSatisfy` aroundY
    it "knows all X rotations are around X axis" $
      forAll (elements rxs) $ \r -> r `shouldSatisfy` aroundX

  describe "rotBy" $ do
    describe "around the Z axis" $ do
      itBehavesLikeRotatingAroundAxis rzs (Lens pointZ)
    describe "around the X axis" $ do
      itBehavesLikeRotatingAroundAxis rxs (Lens pointX)
    describe "around the Y axis" $ do
      itBehavesLikeRotatingAroundAxis rys (Lens pointY)

    describe "non-identity rotations" $ do
      -- named, so we can debug failures better
      let rots = tail rzs <> tail rxs <> tail rys

      -- Since points are not sufficient (and even random polygons
      -- are not enough - consider lines flipped under rotations),
      -- we consider cubes instead.
      specify "all non-identity rotations are distinct" $
        withMaxSuccess 1000 $
        forAll (elements rots)                        $ \r0 ->
        forAll (elements rots `suchThat` (/= r0))     $ \r1 ->
          rcube r0 unitCube `shouldNotBe` rcube r1 unitCube

itBehavesLikeRotatingAroundAxis [r0, r1, r2, r3] axis = do
  describe "r0" $ do
    it "is the identity rotation" $ property $ \p -> do
      rotBy r0 p `shouldBe` p

  describe "quarter turn" (itBehavesLikeAnAxisRotation 4 r1)
  describe "half turn" (itBehavesLikeAnAxisRotation 2 r2)
  describe "three quarter turn" (itBehavesLikeAnAxisRotation 4 r3)

  where
    itChangesTheInput r = it "changes the input" $
      forAll (arbitrary `suchThat` ((/= origin) . set (runLens axis) 0 . nonOrigin)) $ \(NonOrigin p) ->
        rotBy r p `shouldNotBe` p

    itHasRotationalSymmetry n r = it ("has rotational symmetry after " <> show n <> " applications") $ property $ \p ->
      let ps = iterate (rotBy r) p
          p'  = ps !! n
          p'' = ps !! (2 * n)
       in (p', p'') `shouldBe` (p, p)

    itBehavesLikeAnAxisRotation n r = do
      itChangesTheInput r
      itHasRotationalSymmetry n r
      it "does not change the axis of rotation" $ preserveInvariant r (view $ runLens axis)
      it "does not change the magnitude"        $ preserveInvariant r magnitude

    preserveInvariant r f = property $ \p -> f (rotBy r p) `shouldBe` f p

rcube r (a, b, c, d, e, f, g, h) = (rotBy r a, rotBy r b, rotBy r c, rotBy r d
                                   ,rotBy r e, rotBy r f, rotBy r g, rotBy r h
                                   )

rotBy :: Rotation Int -> Point Int -> Point Int
rotBy = rotateBy

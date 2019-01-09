module Elves.RTreeSpec (spec) where

import Test.Hspec
import Test.QuickCheck

import Elves.RTree

type Dim3 = (Int,Int,Int)
type Dim3Set = RTree (Int,Int,Int) ()

query1 :: Dim3 -> Dim3Set -> [(Dim3,())]
query1 i t = take 1 $ query (i,i) t

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

  describe "insert" $ do
    it "increases size by one" $ property $ \t i -> 
      size t + 1 == size (insert (i :: Dim3) () t)
    specify "insertion means queries are successful" $ property $ \t i ->
      query1 i (insert i () t) == [(i :: Dim3,())]

  describe "maxPageSize" $ do
    let maxRegionSize t = case t of Region _ ts -> maximum (length ts : fmap maxRegionSize ts)
                                    _ -> 0

    specify "after indexing, no region is larger than the max-page-size" $ property $ \t ->
      maxRegionSize (t :: Dim3Set) <= maxPageSize
    specify "after inserting, no region is larger than the max-page-size" $ property $ \(NonEmpty elems) ->
      let t = foldr (\i -> insert (i :: Dim3) ()) Tip elems
       in maxRegionSize t <= maxPageSize


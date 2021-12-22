{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

import Data.Tuple
import Data.Maybe
import           Control.Applicative.Combinators
import qualified Data.Attoparsec.Text as A
import qualified Data.List as L
import           Data.Text (Text)
import qualified Data.Text as T
import           Control.Applicative.Combinators
import           Data.Attoparsec.Text            (decimal)
import           Text.Parser.Char (newline, text)

import Elves
import Elves.Advent
import qualified Elves.BinaryList as BL
import           Elves.BinaryList (ListZ, pair, single, goTo)

type SFN = BL.List Int

main :: IO ()
main = day 18 (parser `sepBy1` newline) pt1 pt2 test
  where
    pt1 = print . magnitude . L.foldl1 addSFN
    pt2 = let f (a, b) = magnitude (addSFN a b)
           in print . maximum . fmap f . (>>= \t -> [t, swap t]) . pairs

addSFN :: SFN -> SFN -> SFN
addSFN a b = reduce (pair a b)

magnitude :: SFN -> Int
magnitude (BL.Single n) = n
magnitude (BL.Pair _ _ a b) = (3 * magnitude a) + (2 * magnitude b)

reduce :: SFN -> SFN
reduce = BL.rezip . go . BL.listz
  where
    go z = maybe z go (explode z <|> split z)

explode :: ListZ Int -> Maybe (ListZ Int)
explode = (boom =<<) . leftmostExplodable
  where
    boom z = do
      let (x, y) = BL.indices z
          (BL.Pair _ _ (BL.Single lv) (BL.Single rv)) = BL.focus z

      let lz = goTo (x - 1) z
      z <- fmap (BL.transform (+ lv)) lz <|> pure z
      let rz = goTo (y + 1) z
      z <- fmap (BL.transform (+ rv)) rz <|> pure z
      z <- goTo x z >>= BL.up
      pure (BL.replace (single 0) z)

split :: ListZ Int -> Maybe (ListZ Int)
split = fmap shatter . leftmostSplittable
  where
    shatter z = let (BL.Single n) = BL.focus z
                    (q, r) = quotRem n 2
                    replacement = pair (single q) (single (q + r))
                in BL.replace replacement z

leftmostExplodable :: ListZ a -> Maybe (ListZ a)
leftmostExplodable = (>>= BL.up) . L.find (\z -> BL.depth z == 4) . BL.locations
  where
    fourDeep = (== 4) . BL.depth
    explodable z = BL.leaf z && fourDeep z && maybe False BL.leaf (BL.right z)

leftmostSplittable :: (Num a, Ord a) => ListZ a -> Maybe (ListZ a)
leftmostSplittable = L.find f . BL.locations
  where
    f z = let (BL.Single n) = BL.focus z in n >= 10

parser :: Parser SFN
parser = compositeNum <|> regularNum
  where
    regularNum = single <$> decimal
    compositeNum = do
      text "["
      a <- parser
      text ","
      b <- parser
      text "]"
      pure (pair a b)

test = do
  let n = single
      (/\) = pair
      a ~~ b = n a /\ n b
      becomes = (,)

  describe "goTo" $ do
    let i = "[[10,20],[[[[[9,8],1],2],3],4]]"
    let Right bl = parseOnly parser i
        z = BL.listz bl

    it "can go to any location" $ do
      forM_ (zip [0..] (BL.focus <$> BL.locations z)) $ \(i, exp) -> do
        fmap BL.focus (goTo i z) `shouldBe` Just exp

    it "jumps between any locations" $ do
      let locs = BL.focus <$> BL.locations z
      let indexed = zip [0..] locs
      let indices = fst <$> indexed

      forM_ ((,) <$> indices <*> indices) $ \(i, j) ->
        fmap BL.focus (goTo i z >>= goTo j) `shouldBe` Just (locs !! j)

  describe "depths of locations" $ do
    it "annotates the nodes with the correct depth" $ do
      let i = "[[1,2],[[[[[9,8],1],2],3],4]]"
      let Right bl = parseOnly parser i
      fmap BL.depth (BL.locations $ BL.listz bl) `shouldBe` [1, 1, 5, 5, 4, 3, 2, 1]

  describe "magnitude" $ do
    let examples = [ "[[1,2],[[3,4],5]]" `becomes` 143
                   , "[[[[0,7],4],[[7,8],[6,0]]],[8,1]]" `becomes` 1384
                   , "[[[[1,1],[2,2]],[3,3]],[4,4]]" `becomes` 445
                   , "[[[[3,0],[5,3]],[4,4]],[5,5]]" `becomes` 791
                   , "[[[[5,0],[7,4]],[5,5]],[6,6]]" `becomes` 1137
                   , "[[[[8,7],[7,7]],[[8,6],[7,7]]],[[[0,7],[6,6]],[8,7]]]" `becomes` 3488
                   ]
    forM_ examples $ \(i, r) -> it ("is " <> show r <> " for " <> T.unpack i) $ do
      let Right bl = parseOnly parser i
      magnitude bl `shouldBe` r

  it "can solve pt2" $ do
    let nums = [ "[[[0,[5,8]],[[1,7],[9,6]]],[[4,[1,2]],[[1,4],2]]]"
               , "[[[5,[2,8]],4],[5,[[9,9],0]]]"
               , "[6,[[[6,2],[5,6]],[[7,6],[4,7]]]]"
               , "[[[6,[0,7]],[0,9]],[4,[9,[9,0]]]]"
               , "[[[7,[6,4]],[3,[1,3]]],[[[5,5],1],9]]"
               , "[[6,[[7,3],[3,2]]],[[[3,8],[5,7]],4]]"
               , "[[[[5,4],[7,7]],8],[[8,3],8]]"
               , "[[9,3],[[9,9],[6,[4,9]]]]"
               , "[[2,[[7,7],7]],[[5,8],[[9,3],[0,2]]]]"
               , "[[[[5,2],5],[8,[3,7]]],[[5,[7,5]],[4,4]]]"
               ]
    let Right sfns = mapM (parseOnly parser) nums
    let f (a, b) = magnitude (addSFN a b)
    let maxMag = maximum . fmap f . (>>= \t -> [t, swap t]) . pairs $ sfns

    maxMag `shouldBe` 3993

  describe "addition" $ do
    let examples = [("[[[0,[4,5]],[0,0]],[[[4,5],[2,6]],[9,5]]]"
                    ,"[7,[[[3,7],[4,3]],[[6,3],[8,8]]]]"
                    ,"[[[[4,0],[5,4]],[[7,7],[6,0]]],[[8,[7,7]],[[7,9],[5,0]]]]"
                    )
                   ,("[[[[4,0],[5,4]],[[7,7],[6,0]]],[[8,[7,7]],[[7,9],[5,0]]]]"
                    ,"[[2,[[0,8],[3,4]]],[[[6,7],1],[7,[1,6]]]]"
                    ,"[[[[6,7],[6,7]],[[7,7],[0,7]]],[[[8,7],[7,7]],[[8,8],[8,0]]]]"
                    )
                   ,("[[[[6,7],[6,7]],[[7,7],[0,7]]],[[[8,7],[7,7]],[[8,8],[8,0]]]]"
                    ,"[[[[2,4],7],[6,[0,5]]],[[[6,8],[2,8]],[[2,1],[4,5]]]]"
                    ,"[[[[7,0],[7,7]],[[7,7],[7,8]]],[[[7,7],[8,8]],[[7,7],[8,7]]]]"
                    )
                   ,("[[[[7,0],[7,7]],[[7,7],[7,8]]],[[[7,7],[8,8]],[[7,7],[8,7]]]]"
                    ,"[7,[5,[[3,8],[1,4]]]]"
                    ,"[[[[7,7],[7,8]],[[9,5],[8,7]]],[[[6,8],[0,8]],[[9,9],[9,0]]]]"
                    )
                   ,("[[[[7,7],[7,8]],[[9,5],[8,7]]],[[[6,8],[0,8]],[[9,9],[9,0]]]]"
                    ,"[[2,[2,2]],[8,[8,1]]]"
                    ,"[[[[6,6],[6,6]],[[6,0],[6,7]]],[[[7,7],[8,9]],[8,[8,1]]]]"
                    )
                   ,("[[[[6,6],[6,6]],[[6,0],[6,7]]],[[[7,7],[8,9]],[8,[8,1]]]]"
                    ,"[2,9]"
                    ,"[[[[6,6],[7,7]],[[0,7],[7,7]]],[[[5,5],[5,6]],9]]"
                    )
                   ,("[[[[6,6],[7,7]],[[0,7],[7,7]]],[[[5,5],[5,6]],9]]"
                    ,"[1,[[[9,3],9],[[9,0],[0,7]]]]"
                    ,"[[[[7,8],[6,7]],[[6,8],[0,8]]],[[[7,7],[5,0]],[[5,5],[5,6]]]]"
                    )
                   ,("[[[[7,8],[6,7]],[[6,8],[0,8]]],[[[7,7],[5,0]],[[5,5],[5,6]]]]"
                    ,"[[[5,[7,4]],7],1]"
                    ,"[[[[7,7],[7,7]],[[8,7],[8,7]]],[[[7,0],[7,7]],9]]"
                    )
                   ,("[[[[7,7],[7,7]],[[8,7],[8,7]]],[[[7,0],[7,7]],9]]"
                    ,"[[[[4,2],2],6],[8,7]]"
                    ,"[[[[8,7],[7,7]],[[8,6],[7,7]]],[[[0,7],[6,6]],[8,7]]]"
                    )
                   ]
    forM_ (zip [1..] examples) $ \(i, (a, b, c)) ->
      it (show i <> ": can add " <> T.unpack a <> " and " <> T.unpack b) $ do 
        let Right la = parseOnly parser a
            Right lb = parseOnly parser b
            Right lc = parseOnly parser c
        addSFN la lb `shouldBe` lc

    describe "summing a list" $ do
      let nums = ["[[[0,[4,5]],[0,0]],[[[4,5],[2,6]],[9,5]]]"
                 ,"[7,[[[3,7],[4,3]],[[6,3],[8,8]]]]"
                 ,"[[2,[[0,8],[3,4]]],[[[6,7],1],[7,[1,6]]]]"
                 ,"[[[[2,4],7],[6,[0,5]]],[[[6,8],[2,8]],[[2,1],[4,5]]]]"
                 ,"[7,[5,[[3,8],[1,4]]]]"
                 ,"[[2,[2,2]],[8,[8,1]]]"
                 ,"[2,9]"
                 ,"[1,[[[9,3],9],[[9,0],[0,7]]]]"
                 ,"[[[5,[7,4]],7],1]"
                 ,"[[[[4,2],2],6],[8,7]]"
                 ]
      let total = "[[[[8,7],[7,7]],[[8,6],[7,7]]],[[[0,7],[6,6]],[8,7]]]" 

      it "can sum a list" $ do
        let Right ls = mapM (parseOnly parser) nums
            Right ltot = parseOnly parser total
        L.foldl1 addSFN ls `shouldBe` ltot

  describe "reduce" $ do
    let a = "[[[[[4,3],4],4],[7,[[8,4],9]]],[1,1]]"
    let b = "[[[[0,7],4],[[7,8],[6,0]]],[8,1]]"
    let Right bl = parseOnly parser a
        Right bl' = parseOnly parser b
    it "reduces the example correctly" $ do
      reduce bl `shouldBe` bl'

  describe "split" $ do
    let shouldSplitTo i o = do
          let Right bl = parseOnly parser i
              Right bl' = parseOnly parser o
              mloc = split (BL.listz bl)
          fmap BL.rezip mloc `shouldBe` Just bl'

    mapM_ (\(i, o) -> it (T.unpack i <> " splits into " <> T.unpack o) (i `shouldSplitTo` o))
      [("[[[[0,7],4],[15,[0,13]]],[1,1]]", "[[[[0,7],4],[[7,8],[0,13]]],[1,1]]")
      ,("[[[[0,7],4],[[7,8],[0,13]]],[1,1]]", "[[[[0,7],4],[[7,8],[0,[6,7]]]],[1,1]]")
      ]

  describe "explode" $ do
    let shouldExplodeTo i o = do
          let Right bl = parseOnly parser i
              Right bl' = parseOnly parser o
              mloc = explode (BL.listz bl)
          fmap BL.rezip mloc `shouldBe` Just bl'

    mapM_ (\(i, o) -> it (T.unpack i <> " becomes " <> T.unpack o) (i `shouldExplodeTo` o))
       [("[[[[[9,8],1],2],3],4]", "[[[[0,9],2],3],4]")
       ,("[7,[6,[5,[4,[3,2]]]]]", "[7,[6,[5,[7,0]]]]")
       ,("[[6,[5,[4,[3,2]]]],1]", "[[6,[5,[7,0]]],3]")
       ,("[[3,[2,[1,[7,3]]]],[6,[5,[4,[3,2]]]]]", "[[3,[2,[8,0]]],[9,[5,[4,[3,2]]]]]")
       ,("[[3,[2,[8,0]]],[9,[5,[4,[3,2]]]]]", "[[3,[2,[8,0]]],[9,[5,[7,0]]]]")
       ]

  describe "leftmostExplodable" $ do
    let shouldFindLocation i (e, is) = do
          let Right bl = parseOnly parser i
              mloc = leftmostExplodable (BL.listz bl)
          fmap BL.focus mloc `shouldBe` Just e
          fmap BL.indices mloc `shouldBe` Just is

    mapM_ (\(i, list, is) -> it (T.unpack i) (i `shouldFindLocation` (list, is)))
      [("[[[[[9,8],1],2],3],4]", 9 ~~ 8, (0, 1))
      ,("[7,[6,[5,[4,[3,2]]]]]", 3 ~~ 2, (4, 5))
      ,("[[6,[5,[4,[3,2]]]],1]", 3 ~~ 2, (3, 4))
      ,("[[3,[2,[1,[7,3]]]],[6,[5,[4,[3,2]]]]]", 7 ~~ 3, (3, 4))
      ]

  describe "leftmostSplittable" $ do
    let shouldFindLocation i (e, is) = do
          let Right bl = parseOnly parser i
              mloc = leftmostSplittable (BL.listz bl)
          fmap BL.focus mloc `shouldBe` Just e
          fmap BL.indices mloc `shouldBe` Just is

    mapM_ (\(i, list, is) -> it (T.unpack i) (i `shouldFindLocation` (list, is)))
      [("[[[[0,7],4],[15,[0,13]]],[1,1]]", single 15, (3, 3))
      ,("[[[[0,7],4],[[7,8],[0,13]]],[1,1]]", single 13, (6, 6))
      ]

  describe "parsing" $ do
    mapM_ (\(i, e) -> it ("can parse " <> T.unpack i) (parseOnly parser i `shouldBe` Right e))
          [ ("[1,2]", (1 ~~ 2) )
          , ("[[1,2],3]", (1 ~~ 2) /\ n 3 )
          , ("[9,[8,7]]", (n 9 /\ (8 ~~ 7)))
          , ("[[1,9],[8,5]]", (1 ~~ 9) /\ (8 ~~ 5) )
          , ("[[[[1,2],[3,4]],[[5,6],[7,8]]],9]"
            , let one_two     = 1 ~~ 2
                  three_four  = 3 ~~ 4
                  five_six    = 5 ~~ 6
                  seven_eight = 7 ~~ 8
                  nine = n 9
               in ((one_two /\ three_four) /\ (five_six /\ seven_eight)) /\ nine
            )
          , ("[[[9,[3,8]],[[0,9],6]],[[[3,7],[4,9]],3]]"
                -- -----   ----- -     ----- -----  -
                --------- ---------   ------------- -
               --------------------- -----------------
            , let three_eight = 3 ~~ 8
                  zero_nine   = 0 ~~ 9
                  three_seven = 3 ~~ 7
                  four_nine   = 4 ~~ 9
                  lhs = (n 9 /\ three_eight) /\ (zero_nine /\ n 6)
                  rhs = (three_seven /\ four_nine) /\ n 3
               in lhs /\ rhs
            )
          , ("[[[[1,3],[5,3]],[[1,3],[8,7]]],[[[4,9],[6,9]],[[8,2],[7,3]]]]"
                 ----- -----   ----- -----     ----- -----   ----- -----
                ------------- -------------   ------------   -----------
               ----------------------------- -----------------------------
              -------------------------------------------------------------
            , let one_three =  1 ~~ 3
                  five_three = 5 ~~ 3
                  eight_seven = 8 ~~ 7
                  four_nine = 4 ~~ 9
                  six_nine = 6 ~~ 9
                  eight_two = 8 ~~ 2
                  seven_three = 7 ~~ 3
               in pair ((one_three /\ five_three) /\ (one_three /\ eight_seven))
                       ((four_nine /\ six_nine)   /\ (eight_two /\ seven_three))
            )
          ]

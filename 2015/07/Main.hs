{-# LANGUAGE OverloadedStrings #-}

import           Data.Attoparsec.Text    (decimal, space)
import qualified Data.Attoparsec.Text    as A
import           Data.Bits
import           Data.Char               (isLetter)
import qualified Data.HashMap.Strict     as M
import qualified Data.HashSet            as S
import           Data.Text               (Text)
import qualified Data.Text               as T
import           Data.Word
import           System.Exit             (die)
import           Text.Parser.Char        (newline, text)
import           Text.Parser.Combinators (choice, sepBy1)

import           Elves
import           Elves.Advent

main = day 7 parser pt1 pt2 test
  where
    getA = getValue (Left "a")
    set k v = M.insert k (Input (Right v))
    pt1 c = print (getA c)
    pt2 c = do let mv = do x <- getA c
                           getA (set "b" x c)
               case mv of
                  Left err -> die err
                  Right v  -> print v

test = do
  describe "pt1" $ do
    let mi = parseOnly parser exampleInput
    let solutions = [("d", 72)
                    ,("e", 507)
                    ,("f", 492)
                    ,("g", 114)
                    ,("h", 65412)
                    ,("i", 65079)
                    ,("x", 123)
                    ,("y", 456)
                    ]
    forM_ solutions $ \(k,v) -> do
      it ("can solve for " <> T.unpack k) $ do
        (mi >>= getValue (Left k)) `shouldBe` Right v

type Current = Word16
type Wire = Text

type Value a = Either Wire a

data Recipe a = Input (Value a)
              | Not (Value a)
              | And (Value a) (Value a)
              | Or (Value a) (Value a)
              | LShift (Value a) Int
              | RShift (Value a) Int
              deriving (Show, Eq)

type Circuit = M.HashMap Wire (Recipe Current)

getValue :: Value Current -> Circuit -> Either String Current
getValue v c = fmap (\(_,_,v) -> v) $ go S.empty v c
  where 
   go s (Right v) m = pure (s, m, v)
   go s (Left k) m = do
    s' <- if S.member k s
             then Left ("cycle detected: " <> T.unpack k)
             else pure (S.insert k s)
    r <- maybe (Left $ "Reference error - could not find " ++ T.unpack k) Right (M.lookup k m)
    (s'', m', v) <- evalRecipe s' m r
    pure (S.delete k s'', M.insert k (Input (Right v)) m', v)

   evalRecipe s m r = case r of
    Input val  -> go s val m
    Not val    -> op complement val
    And a b    -> binop (.&.) a b
    Or a b     -> binop (.|.) a b
    LShift a b -> op (`shiftL` b) a
    RShift a b -> op (`shiftR` b) a
    where
      op f val = fmap (\(s,m,v) -> (s,m,f v)) (go s val m)
      binop f a b = do (s', m', av) <- go s a m
                       (s'', m'', bv) <- go s' b m'
                       pure (s'', m'', f av bv)

parser :: Parser Circuit
parser = fmap M.fromList (wireDef `sepBy1` newline)

wireDef = do
      r <- recipeP
      text " -> "
      w <- wire
      pure (w, r)
  where
    recipeP = choice [Not <$> (text "NOT " *> value)
                     ,LShift <$> value <*> (text " LSHIFT " *> decimal)
                     ,RShift <$> value <*> (text " RSHIFT " *> decimal)
                     ,Or <$> value <*> (text " OR " *> value)
                     ,And <$> value <*> (text " AND " *> value)
                     ,Input <$> value
                     ]
    value = choice [Left <$> wire, Right <$> decimal]
    wire = A.takeWhile1 isLetter

exampleInput = T.unlines
  ["123 -> x"
  ,"456 -> y"
  ,"x AND y -> d"
  ,"x OR y -> e"
  ,"x LSHIFT 2 -> f"
  ,"y RSHIFT 2 -> g"
  ,"NOT x -> h"
  ,"NOT y -> i"
  ,"1674 -> bq"
  ,"ab AND ad -> ae"
  ,"NOT ac -> ad"
  ,"1 AND ht -> hu"
  ]


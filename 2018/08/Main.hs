{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE LambdaCase #-}

import Text.Parser.Combinators
import Control.Applicative
import Data.Semigroup
import Data.Foldable
import Data.Tree
import qualified Data.List.NonEmpty as NE

type Metadata = NE.NonEmpty Int

type MTree = Tree Metadata

newtype Parser a = Parser { runParser :: ([Int] -> ([Int], Maybe a)) }
  deriving (Functor)

instance Applicative Parser where
  pure a = Parser $ \stream -> (stream, Just a)
  Parser pf <*> Parser pa = Parser $ \stream ->
    let (stream', f) = pf stream
        (stream'', a) = pa stream'
     in (stream'', f <*> a)

instance Monad Parser where
  return = pure
  Parser pa >>= f = Parser $ \stream ->
    let (stream', ma) = pa stream
     in case fmap f ma of
       Nothing -> (stream', Nothing)
       Just p -> runParser p stream'

instance Alternative Parser where
  empty = Parser $ \s -> (s, Nothing)
  Parser pa <|> Parser pb = Parser $ \s ->
    let (s', ma) = pa s
     in case ma of
       Nothing -> pb s
       Just _  -> (s', ma)

main :: IO ()
main = do
  stream <- fmap read . words <$> getContents
  case runParser parseTree stream of
    ([], Just t) -> do putStr "SUM META: " >> print (sumMeta t)
                       putStr "NODE VAL: " >> print (nodeValue t)
    (_, Nothing) -> putStrLn "Could not parse stream"
    (s, _)       -> putStrLn $ "Unconsumed output: got " ++ show s

eos :: Parser ()
eos = Parser $ \s -> if null s then (s, Just ())
                               else (s, Nothing)

header :: Parser (Int,Int)
header = (,) <$> next <*> next

next :: Parser Int
next = Parser $ \case [] -> ([], Nothing)
                      (e:es) -> (es, Just e)

parseTree :: Parser MTree
parseTree = do
  (nkids,nmeta) <- header
  kids <- count nkids parseTree
  meta <- count nmeta next
  case NE.nonEmpty meta of
    Nothing -> empty
    Just md -> return (Node md kids)

sumMeta :: MTree -> Int
sumMeta = getSum . foldMap (Sum . sum)

nodeValue :: MTree -> Int
nodeValue = foldTree $ \meta values ->
  case values of
    [] -> sum meta
    _  -> sum $ fmap (resolveNode values . pred) meta
    where
      resolveNode xs n | n < 0          = 0
      resolveNode xs n | n >= length xs = 0
      resolveNode xs n                  = xs !! n

example :: [Int]
example = [2, 3, 0, 3, 10, 11, 12, 1, 1, 0, 1, 99, 2, 1, 1, 2]

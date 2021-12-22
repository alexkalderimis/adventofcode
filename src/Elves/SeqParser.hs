{-# LANGUAGE DeriveFunctor #-}

module Elves.SeqParser where

import Control.Applicative
import Data.Sequence (Seq(Empty, (:<|)))
import qualified Data.Sequence as Seq

data ParseError t = EOS | NoParse | Unexpected t | CustomError String deriving (Show, Eq)

newtype SeqParser t a = SeqParser { runSeqParser :: Seq t -> (Seq t, Either (ParseError t) a) }
  deriving (Functor)

instance Applicative (SeqParser t) where
  pure a = SeqParser $ \ts -> (ts, pure a)
  pf <*> px = SeqParser $ \ts -> let (ts', ef) = runSeqParser pf ts
                                     (ts'', ex) = runSeqParser px ts'
                                  in case (ef, ex) of
                                       (Right f, Right x) -> (ts'', Right (f x))
                                       (Right _, Left e) -> (ts, Left e)
                                       (Left e,  _) -> (ts, Left e)

instance Alternative (SeqParser t) where
  empty = SeqParser $ \ts -> (ts, Left NoParse)
  a <|> b = SeqParser $ \ts -> case runSeqParser a ts of
                                 (ts', Right x) -> (ts', Right x)
                                 _ -> runSeqParser b ts

instance Monad (SeqParser t) where
  return = pure
  p >>= f = SeqParser $ \ts -> let (ts', r) = runSeqParser p ts
                                in case r of Left e -> (ts', Left e)
                                             Right x -> runSeqParser (f x) ts'

instance MonadFail (SeqParser t) where
  fail err = SeqParser $ \ts -> (ts, Left (CustomError err))

token :: SeqParser t t
token = SeqParser $ \ts -> case ts of Empty -> (ts, Left EOS)
                                      a :<| ts' -> (ts', Right a)

expect :: Eq t => t -> SeqParser t ()
expect t = SeqParser $ \ts -> case ts of Empty -> (ts, Left EOS)
                                         t' :<| ts' -> if t' == t then (ts', Right ()) else (ts, Left (Unexpected t'))
                           
eos :: SeqParser t ()
eos = SeqParser $ \ts -> case ts of Empty   -> (ts, Right ())
                                    t :<| _ -> (ts, Left (Unexpected t))


parse :: SeqParser t a -> Seq t -> Either (ParseError t) a
parse p = snd . runSeqParser p

embed :: Either (ParseError t) a -> SeqParser t a
embed result = SeqParser $ \ts -> (ts, result)

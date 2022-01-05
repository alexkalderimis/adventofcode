{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}

import Prelude hiding (zipWith)

import           Control.Applicative.Combinators
import           Data.Attoparsec.Text            (decimal, signed)
import qualified Data.Attoparsec.Text            as A
import qualified Data.HashMap.Strict             as Map
import qualified Data.List                       as L
import qualified Data.Foldable as F
import           Data.Maybe
import           Data.Monoid
import           Data.Ord
import qualified Data.Set                        as S
import           Data.Text                       (Text)
import qualified Data.Text                       as T
import           Text.Parser.Char                (newline)
import qualified Data.Vector as V

import           Test.QuickCheck

import           Elves
import Elves.Collections
import           Elves.Advent
import           Elves.Geometry                  (planePoints)
import           Elves.Math.Expression           (Expr, var)

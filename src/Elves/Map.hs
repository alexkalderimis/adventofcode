module Elves.Map where

import qualified Data.Map.Strict as M
import           Data.Map.Strict (Map)

withDefault :: Ord k => a -> Map k a -> k -> a
withDefault v m k = maybe v id $ M.lookup k m

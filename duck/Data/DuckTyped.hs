{-# LANGUAGE DeriveDataTypeable #-}
module Data.DuckTyped where

import qualified Data.Map as Map
import           Data.Dynamic
import           Control.Lens

newtype Object = Object (Map.Map Key Dynamic)
  deriving (Show, Typeable)

newtype Key = Key TypeRep 
  deriving (Eq, Ord, Show, Typeable)

empty :: Object
empty = Object $ Map.empty

objLens :: Lens Object Object (Maybe Int) Int
objLens = lens getr setr
  where
    getr :: Object -> Maybe Int
    getr (Object map0) = Map.lookup k map0 >>= fromDynamic
    setr :: Object -> Int -> Object
    setr (Object map0) x = Object $ Map.insert k (toDyn x) map0 
    k :: Key
    k = Key $ typeOf (42::Int)

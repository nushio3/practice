{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TypeFamilies #-}
module Data.DuckTyped where

import qualified Data.Map as Map
import           Data.Dynamic
import           Control.Lens

newtype Object = Object (Map.Map TypeRep Dynamic)
  deriving (Show, Typeable)

class Typeable a => KeyType a where
  type ValType a :: *

type Record kt = Lens Object Object (Maybe (ValType kt)) (ValType kt)

empty :: Object
empty = Object $ Map.empty

mkRecord :: forall kt. (KeyType kt, Typeable (ValType kt)) => kt -> Record kt
mkRecord k1 = lens getr setr
  where
    getr :: Object -> Maybe (ValType kt)
    getr (Object map0) = Map.lookup k map0 >>= fromDynamic
    setr :: Object -> (ValType kt) -> Object
    setr (Object map0) x = Object $ Map.insert k (toDyn x) map0 
    k :: TypeRep
    k = typeOf k1

{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TypeFamilies #-}
-- | Keys for flying objects.

module Data.DuckTyped.Flying (speed) where

import           Data.Dynamic
import           Data.DuckTyped



data Speed = Speed deriving Typeable
instance KeyType Speed where type ValType Speed = Int
speed :: Record Speed
speed = mkRecord Speed

{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TypeFamilies #-}
-- | Keys for flying objects.

module Data.DuckTyped.Flying (speed, sound) where

import           Data.Dynamic
import           Data.DuckTyped



data Speed = Speed deriving Typeable
instance KeyType Speed where type ValType Speed = Int
speed :: Record Speed
speed = mkRecord Speed


data Sound = Sound deriving Typeable
instance KeyType Sound where type ValType Sound = String
sound :: Record Sound
sound = mkRecord Sound

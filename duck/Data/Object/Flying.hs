{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TypeFamilies #-}
-- | Keys for flying objects.

module Data.Object.Flying (speed, sound) where

import           Data.Dynamic
import           Data.Object


data Unidentified = Unidentified deriving Typeable
instance KeyType Unidentified where type ValType Unidentified = Bool
unidentified :: Record Unidentified
unidentified = mkRecord Unidentified


data Speed = Speed deriving Typeable
instance KeyType Speed where type ValType Speed = Double
speed :: Record Speed
speed = mkRecord Speed

data Sound = Sound deriving Typeable
instance KeyType Sound where type ValType Sound = String
sound :: Record Sound
sound = mkRecord Sound

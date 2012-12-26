{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TypeFamilies #-}
-- | Keys for wave files.

module Data.DuckTyped.Wav (sound) where

import           Data.Dynamic
import           Data.DuckTyped
import qualified Data.Vector as V




data Sound = Sound deriving Typeable
instance KeyType Sound where type ValType Sound = V.Vector Int16
sound :: Record Sound
sound = mkRecord Sound

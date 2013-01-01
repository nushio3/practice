{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TypeFamilies #-}
-- | Keys for wave files.

module Data.Object.Wav (sound) where

import           Data.Dynamic
import           Data.Object
import qualified Data.Vector as V
import           Data.Int



data Sound = Sound deriving Typeable
instance KeyType Sound where type ValType Sound = V.Vector Int16
sound :: Record Sound
sound = mkRecord Sound

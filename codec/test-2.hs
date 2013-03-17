{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}

import GHC.Exts

import qualified Data.Binary as Bin
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy.Char8 as BS


data CodecProxy (x :: * -> Constraint) = CodecProxy

type family GetConstr p :: ( * -> Constraint)
type instance GetConstr (CodecProxy x) = x

class Codec p where
  type Code p :: *
  encode :: ((GetConstr p) a) => p -> a -> Code p
  decode :: ((GetConstr p) a) => p -> Code p -> Maybe a

instance Codec (CodecProxy Bin.Binary) where
  type Code (CodecProxy Bin.Binary) = BS.ByteString
  encode _ = Bin.encode
  decode _ = Just . Bin.decode


class (Read a, Show a) => ReadShow a where

instance (Read a, Show a) => ReadShow a where



instance Codec (CodecProxy ReadShow) where
  type Code (CodecProxy ReadShow) = String
  encode _ = show
  decode _ = Just . read


suepyon :: (Codec p, (GetConstr p) a, Show a, Show (Code p))
           => p -> a -> IO ()
suepyon px a0 = do
  print a0
  print $ encode px a0
  print $ (decode px $ encode px a0) `asTypeOf` (Just a0)

main :: IO ()
main = do
  suepyon (CodecProxy :: CodecProxy Bin.Binary) (4.2 :: Double)
  suepyon (CodecProxy :: CodecProxy ReadShow) (4.2 :: Double)
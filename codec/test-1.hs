{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}

import GHC.Exts

import qualified Data.Binary as Bin
-- import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy.Char8 as BS


data CodecProxy (x :: * -> Constraint) = CodecProxy

type family GetCodec p :: ( * -> Constraint)
type instance GetCodec (CodecProxy x) = x

class Codec (x :: * -> Constraint) where
  type Code x :: *
  encode :: (x a) => a -> Code x
  decode :: (x a) => Code x -> Maybe a

instance Codec (Bin.Binary) where
  type Code Bin.Binary = BS.ByteString
  encode = Bin.encode
  decode = Just . Bin.decode


suepyon :: (Codec x) => x a => a -> Code x
suepyon = undefined



main :: IO ()
main = print 1 -- $ (encode) (1::Int)
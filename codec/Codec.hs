{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}

module Codec where



import qualified Data.Aeson as Aeson
import qualified Data.Binary as Bin
import qualified Data.ByteString.Lazy.Char8 as BS
import qualified Data.ByteString.Char8 as BSS
import qualified Data.Dynamic as Dyn
import qualified Data.Yaml as Yaml
import           GHC.Exts
import qualified Safe
import           Text.Printf

data Pair (encC :: * -> Constraint) (decC :: * -> Constraint) = Pair
data Solo (codecC :: * -> Constraint) = Solo

type family Encoder c :: (* -> Constraint)
type family Decoder c :: (* -> Constraint)
type instance Encoder (Pair x y) = x
type instance Decoder (Pair x y) = y

type instance Encoder (Solo x) = x
type instance Decoder (Solo x) = x



class Codec codec where
  type Code codec :: *
  encode :: (Encoder codec a) => codec -> a -> Code codec
  decode :: (Decoder codec a) => codec -> Code codec -> Maybe a



codecBinary :: Solo Bin.Binary
codecBinary = Solo

instance Codec (Solo Bin.Binary) where
  type Code (Solo Bin.Binary) = BS.ByteString
  encode _ = Bin.encode
  decode _ x = case Bin.decodeOrFail x of
    Left _ -> Nothing
    Right (_,_,ret) -> Just ret


codecShowRead :: Pair Show Read
codecShowRead = Pair

instance Codec (Pair Show Read) where
  type Code (Pair Show Read) = String
  encode _ = show
  decode _ = Safe.readMay

codecJSON :: Pair Aeson.ToJSON Aeson.FromJSON
codecJSON = Pair

instance Codec (Pair Aeson.ToJSON Aeson.FromJSON) where
  type Code (Pair Aeson.ToJSON Aeson.FromJSON) = BS.ByteString
  encode _ = Aeson.encode
  decode _ = Aeson.decode

data CodecYaml = CodecYaml

codecYaml :: CodecYaml
codecYaml = CodecYaml

type instance Encoder CodecYaml = Aeson.ToJSON
type instance Decoder CodecYaml = Aeson.FromJSON

instance Codec CodecYaml where
  type Code CodecYaml = BSS.ByteString
  encode _ = Yaml.encode
  decode _ = Yaml.decode


codecDynamic :: Solo Dyn.Typeable
codecDynamic = Solo

instance Codec (Solo Dyn.Typeable) where
  type Code (Solo Dyn.Typeable) = Dyn.Dynamic
  encode _ = Dyn.toDyn
  decode _ = Dyn.fromDynamic

testCodec :: (Codec c, Encoder c a, Decoder c a, Show a, Show (Code c))
    => c -> a -> IO ()
testCodec px a0 = do
  printf "input:  %s\n" $ show a0
  printf "encode: %s\n" $ show $ encode px a0
  printf "decode: %s\n" $ show $ (decode px $ encode px a0) `asTypeOf` (Just a0)
  printf "\n"
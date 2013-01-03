{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

import Control.Applicative ((<$>), pure)
import Control.Lens
import Data.Dynamic
import qualified Data.Map as Map
import Data.Ratio
import Unsafe.Coerce

newtype Object = Object{unObject :: Map.Map TypeRep Dynamic}


data Mass = Mass {unMass :: Num a => a}
  deriving (Typeable)

mass :: forall tb. Num tb => Simple Traversal Object tb
mass b2ab obj@(Object maptd) =
  case Map.lookup tag maptd of
    Just dyn -> case fromDynamic dyn of
      Just (Mass b) -> (\b' -> Object $ Map.insert tag (toDyn $ Mass (unsafeCoerce b')) maptd) <$> (b2ab b)
      Nothing -> pure obj       
    Nothing -> pure obj
  where
    tag :: TypeRep
    tag = typeOf (Mass undefined)

insert :: Typeable x => x -> Object -> Object
insert x (Object maptd) = Object $ Map.insert (typeOf x) (toDyn x) maptd


empty, hammer1, hammer2 :: Object
empty = Object $ Map.empty
hammer1 = empty & insert (Mass 30)
hammer2 = over mass (*2) hammer1


main = do
  print "hi"
  print $ empty ^? mass
  print $ fmap (*50)   $ hammer2 ^? mass
  print $ fmap (*50.0) $ hammer2 ^? mass

{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

import Control.Lens
import Data.Dynamic
import Data.Ratio
import Unsafe.Coerce

class Member a where
  type ValType a :: *

data Mass = Mass {unMass :: Num a => a}
  deriving (Typeable)
-- instance Member Mass where
--   type ValType Mass = Num a => a

getMass :: Num a => Dynamic -> Maybe a
getMass x = fmap unMass $ fromDynamic x

getHalfMass :: Fractional a => Dynamic -> Maybe a
getHalfMass x = fmap ((/2).unMass) $ fromDynamic x

twiceMass :: Mass -> Mass
twiceMass (Mass x) = Mass $ 2*x

mass :: forall a. Num a => Simple Lens Mass a
mass = lens gettr settr
  where
    gettr :: Mass -> a
    gettr = unMass
    settr :: Mass -> a -> Mass
    settr _ x = Mass (unsafeCoerce x)


x,y  :: Mass
x = Mass 7
y = over mass (*10) x


dx :: Dynamic
dx = toDyn x
dy :: Dynamic
dy = toDyn y


main = do
  print $ dx
  print $ fmap (6*) $ getMass dx
  print $ fmap (11.2-) $ getMass dx
  print $ fmap ((1%147) * ) $ getHalfMass dx
  print $ fmap (3+) $ getMass (toDyn "39")
  print $ x ^. mass
  print $ y ^. mass

  print $ fmap (6*) $ getMass dy
  print $ fmap (0.0-) $ getMass dy
  print $ fmap (3*) $ getMass $ toDyn $ twiceMass x
  print $ fmap (3e41*) $ getMass $ toDyn $ twiceMass x

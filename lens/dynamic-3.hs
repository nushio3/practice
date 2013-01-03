{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}

import Data.Dynamic
import Data.Ratio

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


x :: Mass
x = Mass 7

dx :: Dynamic
dx = toDyn x


main = do
  print $ dx
  print $ fmap (6*) $ getMass dx
  print $ fmap (11.2-) $ getMass dx
  print $ fmap ((1%147) * ) $ getHalfMass dx
  print $ fmap (3+) $ getMass (toDyn "39")
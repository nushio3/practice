{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}

import Data.Dynamic

class Member a where
  type ValType a :: *

data Mass = Mass {unMass :: Num a => a}
  deriving (Typeable)
-- instance Member Mass where
--   type ValType Mass = Num a => a

getMass :: Num a => Dynamic -> Maybe a
getMass x = fmap unMass $ fromDynamic x


x :: Mass
x = Mass 7

dx :: Dynamic
dx = toDyn x


main = do
  print $ dx
  print $ fmap (6*) $ getMass dx
  print $ fmap (11.2-) $ getMass dx
  print $ fmap (3+) $ getMass (toDyn "39")
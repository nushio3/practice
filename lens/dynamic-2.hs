{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}

import Data.Dynamic

class Member a where
  type ValType a :: *

data Mass = Mass {unMass :: Num a => a}
  deriving (Typeable)
-- instance Member Mass where
--   type ValType Mass = Num a => a

x :: Mass
x = Mass 7

dx :: Dynamic
dx = toDyn x


main = do
  print "hy"
  print $ fmap ((6*) .  unMass ) $ (fromDynamic dx)
  print $ fmap ((11.2-) .  unMass ) $ (fromDynamic dx)

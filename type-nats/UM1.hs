{-# LANGUAGE DataKinds #-}              -- to declare the kinds
{-# LANGUAGE KindSignatures #-}         -- (used all over)
{-# LANGUAGE TypeFamilies #-}           -- for declaring operators + sing family
{-# LANGUAGE TypeOperators #-}          -- for declaring operator
{-# LANGUAGE EmptyDataDecls #-}         -- for declaring the kinds
{-# LANGUAGE GADTs #-}                  -- for examining type nats
{-# LANGUAGE PolyKinds #-}              -- for Sing family
{-# LANGUAGE UndecidableInstances #-}   -- for a bunch of the instances
{-# LANGUAGE FlexibleInstances #-}      -- for kind parameters
{-# LANGUAGE FlexibleContexts #-}       -- for kind parameters
{-# LANGUAGE ScopedTypeVariables #-}    -- for kind parameters
{-# LANGUAGE MultiParamTypeClasses #-}  -- for <=, singRep, SingE
{-# LANGUAGE FunctionalDependencies #-} -- for SingRep and SingE


import GHC.TypeLits
import GHC.Prim(Any)

type family Equals (a :: k) (b :: k) :: Bool
type instance where
  Equals a a = True
  Equals a b = False

data SBool (b :: Bool) = SBool
class InB a where
  inB :: a -> Bool
instance InB (SBool True) where inB _ = True
instance InB (SBool False) where inB _ = False

data U (a :: [ ( *, Nat)] ) val = U val
   deriving Show
 
x :: U '[ '(Int, 42) ] Double
x = U 23
 
y :: U '[ '(String, 72) ] Double
y = convert x
 
 
class Compatible a b where
  convert :: Fractional val => U a val -> U b val
-- 
instance Compatible '[ '(Int, 42) ] '[ '(String, 72) ] where
  convert (U x) = U $ 2*x

main :: IO ()
main = do
  print $ inB (SBool :: SBool (Equals Int Int))
  print $ inB (SBool :: SBool (Equals Int Double))
  print $ y

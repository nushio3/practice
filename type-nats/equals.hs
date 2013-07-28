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



main :: IO ()
main = do
  print $ inB (SBool :: SBool (Equals Int Int))
  print $ inB (SBool :: SBool (Equals Int Double))

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

data Natto

newtype instance Sing (Any :: Bool) = SBool Bool
-- instance SingI True  where sing = SBool True
-- instance SingI False where sing = SBool False
instance SingE (KindParam :: OfKind Bool) where
  type DemoteRep (KindParam :: OfKind Bool) = Bool
  fromSing (SBool b) = b


main :: IO ()
main = return ()

{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds, PolyKinds #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, FlexibleContexts #-}

-- Only for MemberU below, when emulating Monad Transformers
{-# LANGUAGE FunctionalDependencies, UndecidableInstances #-}

-- Open unions (type-indexed co-products) for extensible effects
-- This implementation relies on _closed_ type families added
-- to GHC 7.8
-- It has NO overlapping instances and NO Typeable
-- Alas, the absence of Typeable means the projections and injections
-- generally take linear time.
-- The code illustrate how to use closed type families to
-- disambiguate otherwise overlapping instances.

-- The interface is the same as of other OpenUnion*.hs
module OpenUnion41 (Union, inj, prj, decomp, 
                   Member, MemberU2, weaken
                  ) where

-- The data constructors of Union are not exported

-- Essentially, the nested Either data type
-- t is can be a GADT and hence not necessarily a Functor
data Union (r :: [ * -> * ]) v where
  UNow  :: t v -> Union (t ': r) v
  UNext :: Union r v -> Union (any ': r) v

{-
instance Functor (Union r) where
    {-# INLINE fmap #-}
    fmap f (UNow x)  = UNow (f x)
    fmap f (UNext x) = UNext (fmap f x)
-}

data P (n::Nat) = P

-- injecting/projecting at a specified position P n
class Member' t r (n :: Nat) where
  inj' :: P n -> t v -> Union r v
  prj' :: P n -> Union r v -> Maybe (t v)

instance (r ~ (t ': r')) => Member' t r Z where
  inj' _ = UNow
  prj' _ (UNow x) = Just x
  prj' _ _        = Nothing

instance (r ~ (t' ': r'), Member' t r' n) => Member' t r (S n) where
  inj' _ = UNext . inj' (P::P n)
  prj' _ (UNow _)  = Nothing
  prj' _ (UNext x) = prj' (P::P n) x

-- Previously, I tried
--   Convenient synonym
--   -- (also provides compatibility with other OpenUnion*.hs)
--   type Member t r = Member' t r (FindElem t r)
-- Alas, GHC expands the synonyms very early. So, in all type signatures
-- we see Member' rather than Member.

class (Member' t r (FindElem t r)) => Member t r where
  inj :: t v -> Union r v

  prj :: Union r v -> Maybe (t v)

instance (Member' t r (FindElem t r)) => Member t r where
  inj = inj' (P::P (FindElem t r))
  prj = prj' (P::P (FindElem t r))




{-# INLINE decomp #-}
decomp :: Union (t ': r) v -> Either (Union r v) (t v)
decomp (UNow x)  = Right x
decomp (UNext v) = Left v

weaken :: Union r w -> Union (any ': r) w
weaken = UNext

data Nat = Z | S Nat

-- Find an index of an element in a `list'
-- The element must exist
-- This closed type family disambiguates otherwise overlapping
-- instances
type family FindElem (t :: * -> *) r :: Nat where
  FindElem t (t ': r)  = Z
  FindElem t (any ': r)  = S (FindElem t r)


type family EQU (a :: k) (b :: k) :: Bool where
  EQU a a = True
  EQU a b = False
  
-- This class is used for emulating monad transformers
class Member t r => MemberU2 (tag :: k -> * -> *) (t :: * -> *) r | tag r -> t
instance (MemberU' (EQU t1 t2) tag t1 (t2 ': r)) => MemberU2 tag t1 (t2 ': r)

class Member t r =>
      MemberU' (f::Bool) (tag :: k -> * -> *) (t :: * -> *) r | tag r -> t
instance MemberU' True tag (tag e) (tag e ': r)
instance (Member t (t' ': r), MemberU2 tag t r) =>
           MemberU' False tag t (t' ': r)

{-
-- Alternative design
Our list r is essentially the small Universe. Therefore, we can use
the Typeable-like quality evidence in that universe. So, we can define

-- t is existential
data Union r v where
 Union :: t v -> TRep t r -> Union r v

where
data TRep t r where
 T0 :: TRep t (t ': r)
 TS :: TRep t r -> TRep (any ': r)

Then Member is a type class that produces TRep

To avoid overlapping instances we'll have to use something like FindElem trick
anyway. Therefore, there does not seem to be any gain...
-}


data Maybe1 (c :: * -> *) where
  Nothing1 :: Maybe1 c
  Just1    :: c a -> Maybe1 c           -- existential

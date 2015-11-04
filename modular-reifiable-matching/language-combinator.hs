{-# LANGUAGE ConstraintKinds, DataKinds, DeriveFunctor, FlexibleContexts, FlexibleInstances, FunctionalDependencies, GADTs, KindSignatures, MultiParamTypeClasses, PatternSynonyms, RankNTypes, ScopedTypeVariables, StandaloneDeriving, TupleSections, TypeFamilies, TypeOperators, ViewPatterns #-}

import Control.Lens

data Sum (fs :: [* -> *]) x where
  Here :: f x -> Sum (f ': fs) x
  There :: Sum fs x -> Sum (f ': fs) x

instance Functor (Sum '[]) where
  fmap = error "The type (Sum '[] x) is vacuous, therefore this code should never be reached"

instance (Functor f, Functor (Sum fs)) => Functor (Sum (f ': fs)) where
  fmap f (Here t)  = Here $ fmap f t
  fmap f (There t) = There $ fmap f t


_Here :: Prism' (Sum (f ': fs) x) (f x)
_Here = let a :: Sum (f ': fs) x -> Maybe (f x)
            a (Here x) = Just x
            a _        = Nothing
    in prism' Here a

_There :: Prism' (Sum (f ': fs) x) (Sum fs x)
_There = let a :: Sum (f ': fs) x -> Maybe (Sum fs x)
             a (There x) = Just x
             a _         = Nothing
    in prism' There a


class Elem f fs where
  match :: Prism' (Sum fs x) (f x)

instance Elem f (f ': fs) where
  match = _Here
instance Elem f fs => Elem f (g ': fs) where
  match = _There . match

class Subset fs gs where
  subrep :: Prism' (Sum gs x) (Sum fs x)

-- instance Subset '[] gs where
--     srep = SNil
--
-- instance (Functor f, Mem f gs, fs <: gs) => (f ': fs) <: gs where
--     srep = SCons witness srep
--

type Matches fs a b = Sum fs a -> b

extractAt :: Elem f fs => (Matches fs a b) -> (f a -> b)
extractAt sfun fa = sfun $ (review match) fa

main :: IO ()
main = putStrLn "hw"

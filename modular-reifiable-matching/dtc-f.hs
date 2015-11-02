{-# LANGUAGE ConstraintKinds, DeriveFunctor, FlexibleContexts, FlexibleInstances, FunctionalDependencies, GADTs, KindSignatures, MultiParamTypeClasses, PatternSynonyms, RankNTypes, StandaloneDeriving, TupleSections, TypeFamilies, TypeOperators, UndecidableInstances, ViewPatterns #-}


import Control.Lens

data AddF fh ft x = Here (fh x) | There (ft x)
                                  deriving (Eq, Ord, Show, Functor)

_Here :: Prism' (AddF fh ft x) (fh x)
_Here = let f (Here x) = Just x
            f _        = Nothing
    in prism' Here f

_There :: Prism' (AddF fh ft x) (ft x)
_There = let f (There x) = Just x
             f _        = Nothing
    in prism' There f

data MulF fh ft x = These (fh x) (ft x)
                                  deriving (Eq, Ord, Show, Functor)
_These :: Iso' (MulF fh ft x) (fh x, ft x)
_These = iso (\(These t0 g0) -> (t0, g0)) (\(t0,g0) -> These t0 g0)


type a :+ b = AddF a b
type a :* b = MulF a b

infixr 6 :+
infixr 7 :*

data Fix f where
  In :: {mu :: f (Fix f)} -> Fix f
deriving instance (Show (f (Fix f))) => Show (Fix f)

fix :: Iso' (Fix f) (f (Fix f))
fix = iso mu In

-- (+) part
class Matches t x where
  match :: Prism' x (t x)
class MatchesF t f where
  matchF :: Prism' (f x) (t x)

instance MatchesF t t where
  matchF = simple
instance MatchesF t (AddF t f) where
  matchF = _Here
instance MatchesF t g => MatchesF t (AddF f g) where
  matchF = _There . matchF

instance Matches f (Fix f) where
  match = fix

main :: IO ()
main = print "hi"

{-# LANGUAGE ConstraintKinds, DataKinds, DeriveFunctor, FlexibleContexts, FlexibleInstances, FunctionalDependencies, GADTs, KindSignatures, MultiParamTypeClasses, PatternSynonyms, RankNTypes, ScopedTypeVariables, StandaloneDeriving, TupleSections, TypeFamilies, TypeOperators, UndecidableInstances, ViewPatterns #-}

import Control.Lens

data Sum (fs :: [* -> *]) x where
  Void :: Sum '[] x
  Here :: Functor f => f x -> Sum (f ': fs) x
  There :: Sum fs x -> Sum (f ': fs) x

instance Show x => Show (Sum '[] x) where
  show Void = "Void"

instance (Show (f x), Show (Sum fs x)) => Show (Sum (f ': fs) x) where
  showsPrec n (Here x) = showsPrec n x
  showsPrec n (There x) = showsPrec n x

instance Functor (Sum fs) where
  fmap _ Void      = Void
  fmap f (Here t)  = Here $ fmap f t
  fmap f (There t) = There $ fmap f t


_Here :: Functor f => Prism' (Sum (f ': fs) x) (f x)
_Here = let a :: Sum (f ': fs) x -> Maybe (f x)
            a (Here x) = Just x
            a _        = Nothing
    in prism' Here a

_There :: Functor f => Prism' (Sum (f ': fs) x) (Sum fs x)
_There = let a :: Sum (f ': fs) x -> Maybe (Sum fs x)
             a (There x) = Just x
             a _         = Nothing
    in prism' There a


class Elem f fs where
  match :: Prism' (Sum fs x) (f x)

instance {-# OVERLAPPING #-} Functor f => Elem f (f ': fs) where
  match = _Here
instance {-# OVERLAPPABLE #-} (Functor f, Functor g, Elem f fs) => Elem f (g ': fs) where
  match = _There . match

class Subset fs gs where
  subrep :: Prism' (Sum gs x) (Sum fs x)

instance {-# OVERLAPPING #-} Subset '[] fs where
  subrep = prism' _H _I

instance {-# OVERLAPPABLE #-} (Subset fs gs, Elem f gs) => Subset (f ': fs) gs where
  subrep = undefined


type Matches fs a b = Sum fs a -> b

extractAt :: Elem f fs => (Matches fs a b) -> (f a -> b)
extractAt sfun fa = sfun $ (review match) fa

data Fix f where
  In :: Functor f => {out :: f (Fix f)} -> Fix f
instance (Show (f (Fix f))) => Show (Fix f) where
  showsPrec n (In x) = showsPrec n x

fix :: Functor f => Iso' (Fix f) (f (Fix f))
fix = iso out In

type Lang (fs :: [ * -> * ]) = Fix (Sum fs)
type LangPrism (f :: * -> *) = forall f fs . Elem f fs => Prism' (Lang fs) (f (Lang fs))

fold :: (Sum fs a -> a) -> Lang fs -> a
fold k (In x) = k $ fmap (fold k) x

subFix :: (Subset fs gs) => Lang fs -> Lang gs
subFix = fold (In . review subrep)

-- == Example language == --

-- The Value Functor
data ValueF x = ValueF Int
             deriving (Eq, Ord, Show, Functor)
value :: LangPrism ValueF
value = fix . match
-- smart patterns
pattern Value n <- ((^? value) -> Just (ValueF n)) where
  Value n = value # ValueF n

-- The Tuple Functor
data TupleF x = TupleF [x]
             deriving (Eq, Ord, Show, Functor)
tree :: LangPrism TupleF
tree = fix . match

-- smart patterns
pattern Tuple xs <- ((^? tree) -> Just (TupleF xs)) where
  Tuple xs = tree # TupleF xs

-- The Arithmetic Functor
data ArithF x = ImmF Int | AddF x x | MulF x x
             deriving (Eq, Ord, Show, Functor)
arith :: LangPrism ArithF
arith = fix . match
-- smart patterns
pattern Imm n <- ((^? arith) -> Just (ImmF n)) where
  Imm n = arith # ImmF n
pattern Add a b <- ((^? arith) -> Just (AddF a b)) where
  Add a b = arith # AddF a b
pattern Mul a b <- ((^? arith) -> Just (MulF a b)) where
  Mul a b = arith # MulF a b




type Expr = Lang [TupleF, ArithF]

expr1 :: Expr
expr1 = Tuple [Imm 23 `Add` Imm 21, Imm 4, subFix expr2]

expr2 :: Lang '[ ArithF ]
expr2 = Mul (Imm 3) (Imm 41)

main :: IO ()
main = do
  print expr1

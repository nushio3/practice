{-# LANGUAGE ConstraintKinds, DataKinds, DeriveFoldable, DeriveFunctor, DeriveTraversable, FlexibleContexts, FlexibleInstances, FunctionalDependencies, GADTs, KindSignatures, MultiParamTypeClasses, PatternSynonyms, RankNTypes, ScopedTypeVariables, StandaloneDeriving, TupleSections, TypeFamilies, TypeOperators, UndecidableInstances, ViewPatterns #-}

{-

Built based on modular reifiable matching by Bruno C. d. S. Oliveira, Shin-Cheng Mu, and Shu-Hung You.
http://www.iis.sinica.edu.tw/~scm/2015/mrm/

see also: composable algebra by Bruno & Boya http://emmabypeng.github.io/files/FYP_part2.pdf

Tested on stack lts-3.11
-}

import Control.Lens
import Data.Traversable

-- The sum of functors
data Sum (fs :: [* -> *]) x where
  Void :: Sum '[] x
  Here :: Traversable f => f x -> Sum (f ': fs) x
  There :: Sum fs x -> Sum (f ': fs) x

instance Show x => Show (Sum '[] x) where
  show Void = "∅"

instance (Show (f x), Show (Sum fs x)) => Show (Sum (f ': fs) x) where
  showsPrec n (Here x) = showsPrec n x
  showsPrec n (There x) = showsPrec n x

instance Functor (Sum fs) where
  fmap _ Void      = Void
  fmap f (Here t)  = Here $ fmap f t
  fmap f (There t) = There $ fmap f t

instance Foldable (Sum fs) where
  foldMap = foldMapDefault

instance Traversable (Sum fs) where
  traverse afb Void      = pure Void
  traverse afb (Here x)  = Here  <$> traverse afb x
  traverse afb (There x) = There <$> traverse afb x

-- The prisms for accessing here and there
_Here :: Traversable f => Prism' (Sum (f ': fs) x) (f x)
_Here = let a :: Sum (f ': fs) x -> Maybe (f x)
            a (Here x) = Just x
            a _        = Nothing
    in prism' Here a

_There :: Traversable f => Prism' (Sum (f ': fs) x) (Sum fs x)
_There = let a :: Sum (f ': fs) x -> Maybe (Sum fs x)
             a (There x) = Just x
             a _         = Nothing
    in prism' There a


-- The constraint that functor f is an element of a functor set fs
class Elem f fs where
  constructor :: Prism' (Sum fs x) (f x)

instance {-# OVERLAPPING #-} Traversable f => Elem f (f ': fs) where
  constructor = _Here
instance {-# OVERLAPPABLE #-} (Traversable f, Traversable g, Elem f fs) => Elem f (g ': fs) where
  constructor = _There . constructor

-- The constraint that set of functors fs is a subset of gs
class Subset fs gs where
  subrep :: Prism' (Sum gs x) (Sum fs x)

instance {-# OVERLAPPING #-} Subset '[] '[] where
  subrep = simple

instance {-# OVERLAPPING #-} Subset '[] fs => Subset '[] (f ': fs) where
  subrep = prism' There (const Nothing) . subrep

instance {-# OVERLAPPABLE #-} (Traversable f, Elem f gs, Subset fs gs) => Subset (f ': fs) gs where
  subrep = let fwd :: Sum (f ': fs) x -> Sum gs x
               fwd (Here x)  = review constructor x
               fwd (There x) = review subrep x

               bwd :: Sum gs x -> Maybe (Sum (f ': fs) x)
               bwd ((^? constructor ) -> Just x) = Just (Here x)
               bwd ((^? subrep) -> Just x) = Just (There x)
               bwd _                     = Nothing
           in prism' fwd bwd

-- The constraint that object x can somehow be matched to functor f
class Matches f x where
  type Content f x :: *
  match :: Prism' x (f (Content f x))

instance Elem f fs => Matches f (Sum fs x) where
  type Content f (Sum fs x) = x
  match = constructor

instance Elem f fs => Matches f (Fix (Sum fs)) where
  type Content f (Fix (Sum fs)) = Fix (Sum fs)
  match = fix . constructor

instance Matches f (g x) => Matches f (TaggedN g x) where
  type Content f (TaggedN g x) = Content f (g x)
  match = taggedN . match


-- emulate some mrm
type MRM_Matches fs a b = Sum fs a -> b

extractAt :: Elem f fs => (MRM_Matches fs a b) -> (f a -> b)
extractAt sfun fa = sfun $ (review constructor) fa

-- The fix point
data Fix f where
  In :: Functor f => {out :: f (Fix f)} -> Fix f
instance (Show (f (Fix f))) => Show (Fix f) where
  showsPrec n (In x) = showsPrec n x

fix :: Functor f => Iso' (Fix f) (f (Fix f))
fix = iso out In

-- We create languages by folding over set of functors
type Lang (fs :: [ * -> * ]) = Fix (Sum fs)
type MatchPrism (f :: * -> *) = forall f x . Matches f x => Prism' x (f (Content f x))

fold :: (Sum fs a -> a) -> Lang fs -> a
fold k (In x) = k $ fmap (fold k) x

subFix :: (Subset fs gs) => Lang fs -> Lang gs
subFix = fold (In . review subrep)

subOp :: (Subset fs gs) => (Lang gs -> c) -> Lang fs -> c
subOp g = g . subFix

-- ==== Example language ==== --

-- == The Value Functor ==
data ValueF x = ValueF Int
             deriving (Eq, Ord, Show, Functor, Foldable, Traversable)
value :: MatchPrism ValueF
value = match
-- smart patterns
pattern Value n <- ((^? value) -> Just (ValueF n)) where
  Value n = value # ValueF n

-- == The Tuple Functor ==
data TupleF x = TupleF [x]
             deriving (Eq, Ord, Show, Functor, Foldable, Traversable)
tree :: MatchPrism TupleF
tree = match

-- smart patterns
pattern Tuple xs <- ((^? tree) -> Just (TupleF xs)) where
  Tuple xs = tree # TupleF xs

-- == The Arithmetic Functor ==
data ArithF x = ImmF Int | AddF x x | MulF x x
             deriving (Eq, Ord, Show, Functor, Foldable, Traversable)
arith :: MatchPrism ArithF
arith = match
-- smart patterns
pattern Imm n <- ((^? arith) -> Just (ImmF n)) where
  Imm n = arith # ImmF n
pattern Add a b <- ((^? arith) -> Just (AddF a b)) where
  Add a b = arith # AddF a b
pattern Mul a b <- ((^? arith) -> Just (MulF a b)) where
  Mul a b = arith # MulF a b

-- == The Compiler Metadata Functor ==
type Metadata = String
data TaggedF x = TaggedF Metadata x
             deriving (Eq, Ord, Show, Functor, Foldable, Traversable)
tag :: MatchPrism TaggedF
tag = match
-- smart patterns
pattern Tag s x <- ((^? tag) -> Just (TaggedF s x)) where
  Tag s x = tag # TaggedF s x

-- the natural transformation that tagges a functor.
data TaggedN f x = TaggedN Metadata (f x)
             deriving (Eq, Ord, Show, Functor)

taggedN :: Iso' (TaggedN f x)  (f x)
taggedN = iso (\(TaggedN _ x) -> x) (\x -> TaggedN "" x)

-- == type synonyms and evaluation ==

type TaggedExpr = Lang [TaggedF, ArithF, TupleF]
expr1 :: TaggedExpr
expr1 = Tag "1:0" $ Tuple [Imm 23 `Add` Imm 21, Imm 4]


main :: IO ()
main = do
  print expr1

{-
TupleF [AddF (ImmF 23) (ImmF 21),ImmF 4,MulF (ImmF 3) (ImmF 41)]
TupleF [ValueF 4400,ValueF 400,ValueF 12300]
ValueF 123
-}

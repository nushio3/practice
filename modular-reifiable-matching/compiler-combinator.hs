{-# LANGUAGE ConstraintKinds, DataKinds, DeriveFunctor, DeriveFoldable, DeriveTraversable, FlexibleContexts, FlexibleInstances, FunctionalDependencies, GADTs, KindSignatures, MultiParamTypeClasses, PatternSynonyms, RankNTypes, ScopedTypeVariables, StandaloneDeriving, TupleSections, TypeFamilies, TypeOperators, UndecidableInstances, ViewPatterns #-}

{-

Built based on modular reifiable matching by Bruno C. d. S. Oliveira, Shin-Cheng Mu, and Shu-Hung You.
http://www.iis.sinica.edu.tw/~scm/2015/mrm/

Tested on stack lts-3.11

-}

import Control.Lens
import Control.Monad.Trans.Either
import Control.Monad.Reader hiding (fix)
import Data.Traversable

-- The sum of functors
data Sum (fs :: [* -> *]) x where
  Void :: Sum '[] x
  Here :: Functor f => f x -> Sum (f ': fs) x
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

-- The prisms for accessing here and there
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

-- The constraint that functor f is an element of a functor set fs
class Elem f fs where
  match :: Prism' (Sum fs x) (f x)

instance {-# OVERLAPPING #-} Functor f => Elem f (f ': fs) where
  match = _Here
instance {-# OVERLAPPABLE #-} (Functor f, Functor g, Elem f fs) => Elem f (g ': fs) where
  match = _There . match

-- The constraint that set of functors fs is a subset of gs
class Subset fs gs where
  subrep :: Prism' (Sum gs x) (Sum fs x)

instance {-# OVERLAPPING #-} Subset '[] '[] where
  subrep = simple

instance {-# OVERLAPPING #-} Subset '[] fs => Subset '[] (f ': fs) where
  subrep = prism' There (const Nothing) . subrep

instance {-# OVERLAPPABLE #-} (Functor f, Elem f gs, Subset fs gs) => Subset (f ': fs) gs where
  subrep = let fwd :: Sum (f ': fs) x -> Sum gs x
               fwd (Here x)  = review match x
               fwd (There x) = review subrep x

               bwd :: Sum gs x -> Maybe (Sum (f ': fs) x)
               bwd ((^? match ) -> Just x) = Just (Here x)
               bwd ((^? subrep) -> Just x) = Just (There x)
               bwd _                     = Nothing
           in prism' fwd bwd

-- emulate some mrm
type MRM_Matches fs a b = Sum fs a -> b

extractAt :: Elem f fs => (MRM_Matches fs a b) -> (f a -> b)
extractAt sfun fa = sfun $ (review match) fa

-- The fix point
data Fix f where
  In :: Functor f => {out :: f (Fix f)} -> Fix f
instance (Show (f (Fix f))) => Show (Fix f) where
  showsPrec n (In x) = showsPrec n x

fix :: Functor f => Iso' (Fix f) (f (Fix f))
fix = iso out In

-- We create languages by folding over set of functors
type Lang (fs :: [ * -> * ]) = Fix (Sum fs)
type LangPrism (f :: * -> *) = forall f fs . Elem f fs => Prism' (Lang fs) (f (Lang fs))

fold :: (Sum fs a -> a) -> Lang fs -> a
fold k (In x) = k $ fmap (fold k) x

subFix :: (Subset fs gs) => Lang fs -> Lang gs
subFix = fold (In . review subrep)

subOp :: (Subset fs gs) => (Lang gs -> c) -> Lang fs -> c
subOp g = g . subFix

-- ==== Compiler Monad ==== --

newtype CM a = CM { runM :: ReaderT CompilerStatus (EitherT String IO) a}
data CompilerStatus = CompilerStatus { cursor :: String }

-- ==== Example language ==== --

-- == The Value Functor ==
data ValueF x = ValueF Int
             deriving (Eq, Ord, Show, Functor, Foldable, Traversable)
value :: LangPrism ValueF
value = fix . match
-- smart patterns
pattern Value n <- ((^? value) -> Just (ValueF n)) where
  Value n = value # ValueF n

-- == The Tuple Functor ==
data TupleF x = TupleF [x]
             deriving (Eq, Ord, Show, Functor, Foldable, Traversable)
tree :: LangPrism TupleF
tree = fix . match

-- smart patterns
pattern Tuple xs <- ((^? tree) -> Just (TupleF xs)) where
  Tuple xs = tree # TupleF xs

-- == The Arithmetic Functor ==
data ArithF x = ImmF Int | AddF x x | MulF x x
             deriving (Eq, Ord, Show, Functor, Foldable, Traversable)
arith :: LangPrism ArithF
arith = fix . match
-- smart patterns
pattern Imm n <- ((^? arith) -> Just (ImmF n)) where
  Imm n = arith # ImmF n
pattern Add a b <- ((^? arith) -> Just (AddF a b)) where
  Add a b = arith # AddF a b
pattern Mul a b <- ((^? arith) -> Just (MulF a b)) where
  Mul a b = arith # MulF a b

-- == Tag
data TagF x = TagF String x
          deriving (Eq, Ord, Show, Functor, Foldable, Traversable)
tag :: LangPrism TagF
tag = fix . match
-- smart patterns
pattern Tag s x <- ((^? tag) -> Just (TagF s x)) where
  Tag s x = tag # TagF s x

-- == type synonyms and evaluation ==
type Expr = Lang [ArithF, TupleF]
type TV   = Lang [ValueF, TupleF]

eval :: Expr -> TV
eval (Imm n)    = Value n
eval (Add a b)  = evalBin (+) (eval a) (eval b)
eval (Mul a b)  = evalBin (*) (eval a) (eval b)
eval (Tuple xs) = Tuple $ map eval xs

evalBin :: (Int -> Int -> Int) -> TV -> TV -> TV
evalBin  op a1 b1 =
  case (a1,b1) of
   (Tuple xs, Tuple ys) | length xs == length ys ->
                                 Tuple (zipWith (evalBin op) xs ys)
   (Tuple _, Tuple _) -> error "tuple length mismatch"
   (Value x, ys) -> eval1 (op x) ys
   (xs, Value y) -> eval1 (flip op y) xs
   (Value x, Value y) -> Value (op x y)

-- We should be able to traverse over the Fixed structures.
eval1 :: (Int -> Int) -> TV -> TV
eval1 f xs = case xs of
  Value n -> Value (f n)
  Tuple ys -> Tuple $ map (eval1 f) ys




expr1 :: Expr
expr1 = Tuple [Imm 23 `Add` Imm 21, Imm 4, subFix expr2]

expr2 :: Lang '[ ArithF ]
expr2 = Mul (Imm 3) (Imm 41)

expr3 :: Expr
expr3 = Tuple [Imm 100, Imm 1000]

main :: IO ()
main = do
  print expr1
  print $ eval $ Mul (Imm 100) expr1
  print $ subOp eval $ expr2
  print $ eval $ Mul expr1 expr3

{-
TupleF [AddF (ImmF 23) (ImmF 21),ImmF 4,MulF (ImmF 3) (ImmF 41)]
TupleF [ValueF 4400,ValueF 400,ValueF 12300]
ValueF 123
-}

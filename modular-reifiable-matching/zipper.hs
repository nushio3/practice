{-# LANGUAGE ConstraintKinds, DataKinds, DeriveFunctor, FlexibleContexts, FlexibleInstances, FunctionalDependencies, GADTs, KindSignatures, MultiParamTypeClasses, PatternSynonyms, RankNTypes, ScopedTypeVariables, StandaloneDeriving, TupleSections, TypeFamilies, TypeOperators, UndecidableInstances, ViewPatterns #-}

{-

Built based on modular reifiable matching by Bruno C. d. S. Oliveira, Shin-Cheng Mu, and Shu-Hung You.
http://www.iis.sinica.edu.tw/~scm/2015/mrm/

see also: composable algebra by Bruno & Boya http://emmabypeng.github.io/files/FYP_part2.pdf

Tested on stack lts-3.11
-}

import Control.Lens

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
  constructor :: Prism' (Sum fs x) (f x)

instance {-# OVERLAPPING #-} Functor f => Elem f (f ': fs) where
  constructor = _Here
instance {-# OVERLAPPABLE #-} (Functor f, Functor g, Elem f fs) => Elem f (g ': fs) where
  constructor = _There . constructor

-- The constraint that set of functors fs is a subset of gs
class Subset fs gs where
  subrep :: Prism' (Sum gs x) (Sum fs x)

instance {-# OVERLAPPING #-} Subset '[] '[] where
  subrep = simple

instance {-# OVERLAPPING #-} Subset '[] fs => Subset '[] (f ': fs) where
  subrep = prism' There (const Nothing) . subrep

instance {-# OVERLAPPABLE #-} (Functor f, Elem f gs, Subset fs gs) => Subset (f ': fs) gs where
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

instance Elem f fs => Matches f (Zipper (Sum fs) x) where
  type Content f (Zipper (Sum fs) x) = x
  match = prism' (\x -> Zipper [] x) (Just . _zipperHead) . constructor

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

-- a retarded zipper
data Zipper f a = Zipper {_zipperHistory :: Metadata, _zipperHead :: f a}

type ZAlgebrogen f a b = Zipper f a -> b
type ZAlgebra f a = Zipper f a -> a

(+::) :: ZAlgebrogen f a b -> ZAlgebrogen (Sum fs) a b -> ZAlgebrogen (Sum (f ': fs)) a b
af +:: afs = affs
  where
    affs (Zipper h (Here x))  = af  (Zipper h x)
    affs (Zipper h (There x)) = afs (Zipper h x)

(>::) :: Elem f fs => ZAlgebrogen f a b -> ZAlgebrogen (Sum fs) a b -> ZAlgebrogen (Sum fs) a b
(>::) af afs (Zipper h x) = affs x
  where
    affs ((^? constructor) -> Just fa) = af  (Zipper h fa)
    affs x                             = afs (Zipper h x)

zfold :: Elem TaggedF fs => ZAlgebra (Sum fs) a -> Fix (Sum fs) -> a
zfold k x = go k "" x
  where
    go k hist (Tag s x) = k $ Zipper hist (fmap (go k s) (out x))
    go k hist x         = k $ Zipper hist (fmap (go k hist) (out x))

subFix :: (Subset fs gs) => Lang fs -> Lang gs
subFix = fold (In . review subrep)

subOp :: (Subset fs gs) => (Lang gs -> c) -> Lang fs -> c
subOp g = g . subFix

-- ==== Example language ==== --

-- == The Value Functor ==
data ValueF x = ValueF Int
             deriving (Eq, Ord, Show, Functor)
value :: MatchPrism ValueF
value = match
-- smart patterns
pattern Value n <- ((^? value) -> Just (ValueF n)) where
  Value n = value # ValueF n

-- == The Tuple Functor ==
data TupleF x = TupleF [x]
             deriving (Eq, Ord, Show, Functor)
tree :: MatchPrism TupleF
tree = match

-- smart patterns
pattern Tuple xs <- ((^? tree) -> Just (TupleF xs)) where
  Tuple xs = tree # TupleF xs

-- == The Arithmetic Functor ==
data ArithF x = ImmF Int | AddF x x | MulF x x
             deriving (Eq, Ord, Show, Functor)
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
             deriving (Eq, Ord, Show, Functor)
tag :: MatchPrism TaggedF
tag = match
-- smart patterns
pattern Tag s x <- ((^? tag) -> Just (TaggedF s x)) where
  Tag s x = tag # TaggedF s x

-- Find tag from history of the zipper
location :: Elem TaggedF fs => Zipper (Sum fs) x -> Metadata
location (Zipper h _) = h

propagateTag :: Elem TaggedF gs => ZAlgebra TaggedF (Lang gs)
propagateTag (Zipper _ (TaggedF s x)) = Tag s x


-- == type synonyms and evaluation ==
type Expr = Lang [ArithF, TupleF]
type TV   = Lang [ValueF, TupleF]

evalArith :: Elem ValueF gs => ZAlgebra ArithF (Lang gs)
evalArith (Imm n)    = Value n
evalArith (Add a b)  = evalBin (+) a b
evalArith (Mul a b)  = evalBin (*) a b

eval (Tuple xs) = Tuple $ map eval xs

evalBin :: (Elem ValueF gs, Elem TupleF gs) => (Int -> Int -> Int) -> Lang gs -> Lang gs -> Lang gs
evalBin  op a1 b1 =
  case (a1,b1) of
  -- Oh, we cannot access zipper from here!!
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
expr3 = Tuple [Imm 23, Imm 21, Imm 4, Imm 4]


main :: IO ()
main = do
  print "hi"

{-
TupleF [AddF (ImmF 23) (ImmF 21),ImmF 4,MulF (ImmF 3) (ImmF 41)]
TupleF [ValueF 4400,ValueF 400,ValueF 12300]
ValueF 123
-}

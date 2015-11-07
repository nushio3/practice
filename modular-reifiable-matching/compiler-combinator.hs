{-# LANGUAGE ConstraintKinds, DataKinds, DeriveFunctor, DeriveFoldable, DeriveTraversable, FlexibleContexts, FlexibleInstances, FunctionalDependencies, GADTs, KindSignatures, MultiParamTypeClasses, PatternSynonyms, RankNTypes, ScopedTypeVariables, StandaloneDeriving, TemplateHaskell, TupleSections, TypeFamilies, TypeOperators, UndecidableInstances, ViewPatterns #-}

{-

Built based on modular reifiable matching by Bruno C. d. S. Oliveira, Shin-Cheng Mu, and Shu-Hung You.
http://www.iis.sinica.edu.tw/~scm/2015/mrm/

Tested on stack lts-3.11

-}

import Control.Applicative
import Control.Lens
import Control.Monad.Trans.Either
import Control.Monad.State hiding (fix)
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
  match :: Prism' (Sum fs x) (f x)

instance {-# OVERLAPPING #-} Traversable f => Elem f (f ': fs) where
  match = _Here
instance {-# OVERLAPPABLE #-} (Traversable f, Traversable g, Elem f fs) => Elem f (g ': fs) where
  match = _There . match

-- The constraint that set of functors fs is a subset of gs
class Subset fs gs where
  subrep :: Prism' (Sum gs x) (Sum fs x)

instance {-# OVERLAPPING #-} Subset '[] '[] where
  subrep = simple

instance {-# OVERLAPPING #-} Subset '[] fs => Subset '[] (f ': fs) where
  subrep = prism' There (const Nothing) . subrep

instance {-# OVERLAPPABLE #-} (Traversable f, Elem f gs, Subset fs gs) => Subset (f ': fs) gs where
  subrep = let fwd :: Sum (f ': fs) x -> Sum gs x
               fwd (Here x)  = review match x
               fwd (There x) = review subrep x

               bwd :: Sum gs x -> Maybe (Sum (f ': fs) x)
               bwd ((^? match ) -> Just x) = Just (Here x)
               bwd ((^? subrep) -> Just x) = Just (There x)
               bwd _                       = Nothing
           in prism' fwd bwd

-- emulate some mrm
type Matches fs a b = Sum fs a -> b

extractAt :: Elem f fs => Matches fs a b -> (f a -> b)
extractAt sfun fa = sfun $ (review match) fa

-- The fix point
data Fix f where
  In :: Traversable f => {out :: f (Fix f)} -> Fix f
instance (Show (f (Fix f))) => Show (Fix f) where
  showsPrec n (In x) = showsPrec n x

fix :: Traversable f => Iso' (Fix f) (f (Fix f))
fix = iso out In

-- We create languages by folding over set of functors
type Lang (fs :: [ * -> * ]) = Fix (Sum fs)
type LangPrism (f :: * -> *) = forall f fs . Elem f fs => Prism' (Lang fs) (f (Lang fs))
type Algebras fs a = Sum fs a -> a

fold :: (Sum fs a -> a) -> Lang fs -> a
fold k (In x) = k $ fmap (fold k) x

subFix :: (Subset fs gs) => Lang fs -> Lang gs
subFix = fold (In . review subrep)

subOp :: (Subset fs gs) => (Lang gs -> c) -> Lang fs -> c
subOp g = g . subFix

mlift :: Monad m => Matches fs a b -> Matches fs (m a) (m b)
mlift fsa2b fsma = liftM fsa2b $ sequence fsma

foldM :: Monad m => (Sum fs a -> m a) -> Lang fs -> m a
foldM k x = fold (join . mlift k) x



-- ==== Compiler Monad ==== --

type CM = EitherT String (StateT CompilerState IO)
data CompilerState = CompilerStatus { _cursor :: String }
makeLenses ''CompilerState

throw :: String -> CM a
throw msg = do
  s <- use cursor
  left $ "Compiler error at " ++ s ++ "\n" ++ msg

runCompiler :: CM a -> IO (Either String a)
runCompiler cm = do
  ret <- flip evalStateT (CompilerStatus "somewhere in the program") $ runEitherT cm
  return ret

testCompiler :: Show a => CM a -> IO ()
testCompiler cm = do
  ret <- runCompiler cm
  case ret of
   Left err -> putStrLn err
   Right a -> print a

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

withTag :: Subset fs fs => Matches fs a (CM a) -> Matches (TagF ': fs) a (CM a)
withTag m (Here (TagF s x)) = do
  cursor .= s
  return x
withTag m (There x) = m $ review subrep x




-- == type synonyms and evaluation ==
type TaggedExpr = Lang [TagF, ArithF, TupleF]
type Expr = Lang [ArithF, TupleF]
type TV   = Lang [ValueF, TupleF]


evArith :: Matches '[ArithF] TV (CM TV)
evArith (Imm n)    = return $ Value n
evArith (Add a b)  = evalBin (+) a b
evArith (Mul a b)  = evalBin (*) a b

evTuple :: Matches '[TupleF] TV (CM TV)
evTuple (TupleF xs) = Tuple xs

evalBin :: (Int -> Int -> Int) -> TV -> TV -> CM TV
evalBin  op a1 b1 =
  case (a1,b1) of
   (Tuple xs, Tuple ys) | length xs == length ys ->
                                 Tuple (zipWith (evalBin op) xs ys)

  {- You should be able to retrieve the tag of (Tuple _)
     clause, which you already have consumed!! -}
   (Tuple _, Tuple _) -> throw "tuple length mismatch"
   (Value x, ys) -> eval1 (op x) ys
   (xs, Value y) -> eval1 (flip op y) xs
   (Value x, Value y) -> return $  Value (op x y)

-- We should be able to traverse over the Fixed structures.
eval1 :: (Int -> Int) -> TV -> CM TV
eval1 f xs = case xs of
  Value n ->  return $ Value (f n)
  Tuple ys -> Tuple <$> mapM (eval1 f) ys

eval :: Expr -> CM TV
eval = undefined

expr1 :: TaggedExpr
expr1 = Tag "1:0" $ Tuple [Imm 23 `Add` Imm 21, Imm 4, subFix expr2]

expr2 :: Lang '[ ArithF ]
expr2 = Mul (Imm 3) (Imm 41)

expr3 :: TaggedExpr
expr3 = Tag "3:0" $ Tuple [Imm 100, Imm 1000]

main :: IO ()
main = do
  print expr1
  testCompiler $ eval $ Mul (Imm 100) expr1
  testCompiler $ subOp eval $ expr2
  testCompiler $ eval $ Mul expr1 expr3

{-
TupleF [AddF (ImmF 23) (ImmF 21),ImmF 4,MulF (ImmF 3) (ImmF 41)]
TupleF [ValueF 4400,ValueF 400,ValueF 12300]
ValueF 123
-}

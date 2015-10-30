{-# LANGUAGE ConstraintKinds, DeriveFunctor, FlexibleContexts, FlexibleInstances, GADTs, KindSignatures, MultiParamTypeClasses, PatternSynonyms, RankNTypes, StandaloneDeriving, TypeOperators, UndecidableInstances, ViewPatterns #-}

import Control.Lens

data AddF fh ft x = Here (fh x) | There (ft x)
                                  deriving (Eq, Ord, Show, Functor)
data MulF fh ft x = These (fh x) (ft x)
                                  deriving (Eq, Ord, Show, Functor)
type a :+ b = AddF a b
type a :* b = MulF a b

infixr 6 :+
infixr 7 :*

data Fix f where
  In :: {mu :: f (Fix f)} -> Fix f
deriving instance (Show (f (Fix f))) => Show (Fix f)

fix :: Iso' (Fix f) (f (Fix f))
fix = iso mu In

-- The (+) part
type AutoPrism t = forall x. Matches t x => Prism' x (t x)
type Matches t x = MatchesF t x x

class MatchesF t x s where
  match :: Prism' s (t x)

instance MatchesF t x (t x) where
  match = simple

instance {-# OVERLAPPING #-} MatchesF t x (AddF t g x) where
  match = let match2 (Here x) = Just x
              match2 _        = Nothing
    in prism' Here match2

instance {-# OVERLAPPABLE #-} MatchesF t x (g x) => MatchesF t x (AddF f g x) where
  match = let match2 (There x) = Just x
              match2 _        = Nothing
    in prism' There match2 . match

instance MatchesF t (Fix f) (f (Fix f)) => MatchesF t (Fix f) (Fix f) where
  match = fix . match


-- The (*) part
{- This is how we can construct values only using lenses:

Prelude Control.Lens> let x = undefined & _1 .~ 2 & _2 .~ "hi":: (Int,String)
Prelude Control.Lens> x
(2,"hi")

Yes, it risks partial computations, but we also do so in case of pattern matching,
and also in Haskell we can leave records uninitialized.
-}

type AutoLens t = forall x. Has t x => Lens' x (t x)
type Has t x = HasF t x x

class HasF t x s where
  have :: Lens' s (t x)

instance HasF t x (t x) where
  have = simple

instance {-# OVERLAPPING #-} HasF t x (MulF t g x) where
  have = let getter (These t0 _) = t0
             setter (These _ tt) t0 = These t0 tt
    in lens getter setter

instance {-# OVERLAPPABLE #-} HasF t x (g x) => HasF t x (MulF f g x) where
  have = let getter (These _ tt) = tt
             setter (These t0 _) tt = These t0 tt
    in lens getter setter . have

instance HasF t (Fix f) (f (Fix f)) => HasF t (Fix f) (Fix f) where
  have = fix . have


-- The Base Functor
data Nil x = Nil
             deriving (Eq, Ord, Show, Functor)

-- The Value Functor
data ValueF x = ValueF Int
             deriving (Eq, Ord, Show, Functor)
value :: AutoPrism ValueF
value = match
-- smart patterns
pattern Value n <- ((^? value) -> Just (ValueF n)) where
  Value n = value # ValueF n


-- The Tree Functor
data TreeF x = BranchF [x]
             deriving (Eq, Ord, Show, Functor)
type MatchesTree x = Matches TreeF x
tree :: AutoPrism TreeF
tree = match

-- smart patterns
pattern Branch xs <- ((^? tree) -> Just (BranchF xs)) where
  Branch xs = tree # BranchF xs


-- The Arithmetic Functor
data ArithF x = ImmF Int | AddF x x | MulF x x
             deriving (Eq, Ord, Show, Functor)
type MatchesArith x = Matches ArithF x

arith :: AutoPrism ArithF
arith = match

-- smart patterns
pattern Imm n <- ((^? arith) -> Just (ImmF n)) where
  Imm n = arith # ImmF n
pattern Add a b <- ((^? arith) -> Just (AddF a b)) where
  Add a b = arith # AddF a b
pattern Mul a b <- ((^? arith) -> Just (MulF a b)) where
  Mul a b = arith # MulF a b

-- The tagging Functor
data TagF x = TagF String
             deriving (Eq, Ord, Show, Functor)
-- type HasTag x = Has TagF x
-- tag :: AutoLens TagF
-- tag = have
--
-- -- smart patterns
-- pattern Tag t r <- ()



type ArithExpr = Fix ArithF

ax :: ArithExpr
ax = In (AddF (In (ImmF 40)) (In (ImmF 2)))

type ATExpr = Fix (TreeF :+ ArithF :+ Nil)
type TV = Fix (TreeF :+ ValueF :+ Nil)

atx :: ATExpr
atx = In (Here (BranchF [In $ There $ Here $ ImmF 42]))

type Expr = Fix (TagF :* (TreeF :+ ArithF :+ Nil) :* Nil)

expr :: Expr
expr = In $ These (TagF "0:0") $ These (There $ Here $ ImmF 42) Nil

main :: IO ()
main = do
  print "hi"
  print ax
  print atx
  print expr
  print $ (atx ^? arith :: Maybe (ArithF ATExpr))
  print $ (arith # ImmF 4242 :: ATExpr)
  print $ [atx ^? tree, Just (BranchF  [atx])]


  let testExpr :: ATExpr
      testExpr = Add (Branch [Imm 3, Add (Imm 6) (Imm 9)]) (Branch [Imm 10, Imm 100])
  print testExpr
  print $ eval testExpr

eval :: ATExpr -> TV
eval (Imm n) = Value n
eval (Add a0 b0) = evalB (+) (eval a0) (eval b0)
eval (Mul a0 b0) = evalB (*) (eval a0) (eval b0)
eval (Branch xs) = Branch $ map eval xs

-- We should be able to traverse over the Fixed structures.
eval1 :: (Int -> Int) -> TV -> TV
eval1 f xs = case xs of
  Value n -> Value (f n)
  Branch ys -> Branch $ map (eval1 f) ys

evalB :: (Int -> Int -> Int) -> TV -> TV -> TV
evalB op a1 b1 =
  case (a1,b1) of
   (Branch xs, Branch ys) | length xs == length ys ->
                                 Branch (zipWith (evalB op) xs ys)
   (Branch _, Branch _) -> error "branch length mismatch"
   (Value x, ys) -> eval1 (op x) ys
   (xs, Value y) -> eval1 (flip op y) xs
   (Value x, Value y) -> Value (op x y)

{-# LANGUAGE ConstraintKinds, DeriveFunctor, FlexibleContexts, FlexibleInstances, GADTs, KindSignatures, MultiParamTypeClasses, OverlappingInstances, RankNTypes, StandaloneDeriving, TypeOperators, UndecidableInstances #-}

import Control.Lens

data AddF fh ft x = Here (fh x) | There (ft x)
                                  deriving (Eq, Ord, Show, Functor)
data MulF fh ft x = Prod (fh x) (ft x)
                                  deriving (Eq, Ord, Show, Functor)
type a :+ b = AddF a b
type a :* b = MulF a b

data Fix f where
  In :: {mu :: f (Fix f)} -> Fix f

fix :: Iso' (Fix f) (f (Fix f))
fix = iso mu In

type Matcher t = forall x. Matches t x x => Prism' x (t x)

class Matches t x a where
  match :: Prism' a (t x)

instance Matches t x (t x) where
  match = simple

instance Matches t x (AddF t g x) where
  match = let match2 (Here x) = Just x
              match2 _        = Nothing
    in prism' Here match2

instance Matches t x (g x) => Matches t x (AddF f g x) where
  match = let match2 (There x) = Just x
              match2 _        = Nothing
    in prism' There match2 . match

instance Matches t (Fix f) (f (Fix f)) => Matches t (Fix f) (Fix f) where
  match = fix . match



data Void x = Void
             deriving (Eq, Ord, Show, Functor)

data TreeF x = Branch [x]
             deriving (Eq, Ord, Show, Functor)
type MatchesTree = Matches TreeF
tree :: Matcher TreeF
tree = match

data ArithF x = Imm Int | Add x x | Mul x x
             deriving (Eq, Ord, Show, Functor)
type MatchesArith = Matches ArithF
arith :: Matcher ArithF
arith = match


data TaggedF x = Tag String
             deriving (Eq, Ord, Show, Functor)

type ExprF = TaggedF :* (TreeF :+ ArithF)


deriving instance (Show (f (Fix f))) => Show (Fix f)

type ArithExpr = Fix ArithF

ax :: ArithExpr
ax = In (Add (In (Imm 40)) (In (Imm 2)))

type ATExpr = Fix (TreeF :+ (ArithF :+ Void))

atx :: ATExpr
atx = In (Here (Branch [In $ There $ Here $ Imm 42]))

type Expr = Fix (TaggedF :* (TreeF :+ ArithF))

expr :: Expr
expr = In $ Prod (Tag "0:0") $ (There $ Imm 42)

main :: IO ()
main = do
  print "hi"
  print ax
  print atx
  print expr
  print $ (atx ^? arith :: Maybe (ArithF ATExpr))
  print $ (arith # Imm 4242 :: ATExpr)
  print $ [atx ^? tree, Just (Branch  [atx])]

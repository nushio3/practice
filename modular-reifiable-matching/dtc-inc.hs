{-# LANGUAGE DeriveFunctor, FlexibleContexts, FlexibleInstances, GADTs, KindSignatures, MultiParamTypeClasses, OverlappingInstances, StandaloneDeriving, TypeOperators, UndecidableInstances #-}

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


data Void x = Void
             deriving (Eq, Ord, Show, Functor)

data TreeF x = Branch [x]
             deriving (Eq, Ord, Show, Functor)

class MatchesTree x a where
  tree :: Prism' a (TreeF x)

instance MatchesTree x (TreeF x) where
  tree = simple

instance MatchesTree x (AddF TreeF g x) where
  tree = let match (Here x) = Just x
             match _        = Nothing
    in prism' Here match

instance MatchesTree x (g x) => MatchesTree x (AddF f g x) where
  tree = let match (There x) = Just x
             match _        = Nothing
    in prism' There match . tree

instance MatchesTree (Fix f) (f (Fix f)) => MatchesTree (Fix f) (Fix f) where
  tree = fix . tree



data ArithF x = Imm Int | Add x x | Mul x x
             deriving (Eq, Ord, Show, Functor)

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
  print $ [atx ^? tree, Just (Branch  [atx])]

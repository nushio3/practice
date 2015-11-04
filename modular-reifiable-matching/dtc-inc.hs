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

-- The (+) part
type AutoPrism t = forall x. Matches t x => Prism' x (t x)
type Matches t x = MatchesF t x x

class MatchesF t x s where
  match :: Prism' s (t x)

instance {-# OVERLAPPING #-} MatchesF t x (AddF t g x) where
  match = _Here

instance {-# OVERLAPPABLE #-} MatchesF t x (g x) => MatchesF t x (AddF f g x) where
  match = _There . match

instance MatchesF t (Fix f) (f (Fix f)) => MatchesF t (Fix f) (Fix f) where
  match = fix . match


-- The (+) part interacting with (*) part

instance {-# OVERLAPPABLE #-} MatchesF t x (f x) => MatchesF t x (MulF f Nil x) where
  match = _These . iso fst (,Nil) . match



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

arith' :: MatchesF ArithF x s => Prism' s (ArithF x)
arith' = match
pattern Imm' n <- ((^? arith') -> Just (ImmF n)) where
  Imm' n = arith' # ImmF n

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

{-
type MakesTag x = Makes TagF x
tag :: AutoPartition TagF
tag = partition

-- smart patterns
pattern r :@ t <- (view tag -> (TagF t, r)) where
  r :@ t = view (from tag) (TagF t, r)
-}


type ArithExpr = Fix ArithF

ax :: ArithExpr
ax = In (AddF (In (ImmF 40)) (In (ImmF 2)))

type ATExpr = Fix (TreeF :+ ArithF :+ Nil)
type TV = Fix (TreeF :+ ValueF :+ Nil)

atx :: ATExpr
atx = In (Here (BranchF [In $ There $ Here $ ImmF 42]))

type Expr = Fix (TagF :* (TreeF :+ ArithF :+ Nil) :* Nil)

expr1 :: Expr
expr1 = In $ These (TagF "0:0") $ These (There $ Here $ ImmF 42) Nil

main :: IO ()
main = do
  print "hi"
  print ax
  print atx
  print expr1
  print (view (fix . _These . _2 ._These . _1) expr1 )
--  print $ expr1 & fix . _These . _2 ._These . _1 .~ Imm 30 -- This does not work.
  print ""

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

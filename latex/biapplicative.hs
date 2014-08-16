{-# LANGUAGE DataKinds, FlexibleContexts, FlexibleInstances, GADTs, KindSignatures, OverloadedStrings, ScopedTypeVariables #-}

import Data.Biapplicative
import Data.Monoid
import Data.String

data Expr (b :: Bool) s a where
  ERep :: { fromERep :: s } -> Expr False s a
  Expr :: { fromExpr :: a } -> Expr True s a

instance Bifunctor (Expr True) where
  bimap _ f (Expr a) = Expr (f a)
instance Bifunctor (Expr False) where
  bimap f _ (ERep s) = ERep (f s)
  

instance Biapplicative (Expr True) where
  bipure s a = Expr a
  ef <<*>> ea = Expr (fromExpr ef $ fromExpr ea)

instance Biapplicative (Expr False) where
  bipure s a = ERep s
  ef <<*>> es = ERep (fromERep ef $ fromERep es)


solarMass :: Expr b String Double
solarMass = bipure "Msun" 2.0e33

main :: IO ()
main = return ()

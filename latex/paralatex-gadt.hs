{-# LANGUAGE DataKinds, FlexibleContexts, GADTs, KindSignatures, OverloadedStrings, ScopedTypeVariables #-}


import Data.Monoid
import Data.String

data Expr (b :: Bool) s a where
  ERep :: { fromERep :: s } -> Expr False s a
  Expr :: { fromExpr :: a } -> Expr True s a




instance (Num a, IsString s, Monoid s) => Num (Expr b s a) where
  ERep s1 + ERep s2 = ERep (s1 <> "+" <> s2)
  Expr a1 + Expr a2 = Expr (a1 + a2)
  ERep s1 * ERep s2 = ERep (s1 <> "*" <> s2)
  Expr a1 * Expr a2 = Expr (a1 * a2)
  

paren :: (IsString s, Monoid s) => Expr b s a ->  Expr b s a 
paren (Expr a) = Expr a
paren (ERep s) = ERep $ "(" <> s <> ")"

momentumX :: (Num a, IsString s, Monoid s) => Expr b s a ->  Expr b s a  ->  Expr b s a 
momentumX mass vel = mass * vel

momentumF :: forall a. Num a => a -> a -> a
momentumF m v = fromExpr $ momentumX (Expr m) (Expr v :: Expr True String a)

momentumE :: String -> String -> String
momentumE m v = fromERep $ momentumX (ERep m) (ERep v :: Expr False String Double)



main :: IO ()
main = do
  putStrLn "hi"
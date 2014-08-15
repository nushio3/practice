{-# LANGUAGE DataKinds, FlexibleContexts, GADTs, KindSignatures, OverloadedStrings #-}


import Data.Monoid
import Data.String

data Expr (b :: Bool) s a where
  ERep :: s -> Expr False s a
  Expr :: a -> Expr True s a


instance (Num a, IsString s, Monoid s) => Num (Expr b s a) where
  ERep s1 + ERep s2 = ERep (s1 <> "+" <> s2)
  Expr a1 + Expr a2 = Expr (a1 + a2)
  ERep s1 * ERep s2 = ERep (s1 <> "*" <> s2)
  Expr a1 * Expr a2 = Expr (a1 * a2)
  

paren :: (IsString s, Monoid s) => Expr b s a ->  Expr b s a 
paren (Expr a) = Expr a
paren (ERep s) = ERep $ "(" <> s <> ")"

main :: IO ()
main = do
  putStrLn "hi"
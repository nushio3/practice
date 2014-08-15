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

instance Num a => Num (Expr True s a) where
  Expr a1 + Expr a2 = Expr (a1 + a2)
  Expr a1 * Expr a2 = Expr (a1 * a2)
  
instance (IsString s, Monoid s) => Num (Expr False s a) where
  ERep s1 + ERep s2 = ERep (s1 <> "+" <> s2)
  ERep s1 * ERep s2 = ERep (s1 <> "*" <> s2)



paren :: (IsString s, Monoid s) => Expr b s a ->  Expr b s a 
paren (Expr a) = Expr a
paren (ERep s) = ERep $ "(" <> s <> ")"

momentumX :: (Num (Expr b s a)) => Expr b s a ->  Expr b s a  ->  Expr b s a 
momentumX mass vel = mass * vel

momentumE :: forall a. Num a => a -> a -> a
momentumE m v = fromExpr $ momentumX (Expr m) (Expr v)

momentumR :: String -> String -> String
momentumR m v = fromERep $ momentumX (ERep m) (ERep v)


solarMass :: Expr b String Double
solarMass = bipure "Msun" 2.0e33

main :: IO ()
main = do
  putStrLn $ "The momentum of particle with mass M, velocity V is : "
    ++ momentumR "M" "V"
  putStrLn $ "Therefore, momentum of particle with mass 6.0, velocity 7.0 is : "
    ++ show (momentumE 6.0 7.0)
    
    
{-

paralatex-gadt.hs:50:13:
    No instance for (Biapplicative (Expr b))
      arising from a use of ‘bipure’
    In the expression: bipure "Msun" 2.0e33
    In an equation for ‘solarMass’: solarMass = bipure "Msun" 2.0e33


-}    
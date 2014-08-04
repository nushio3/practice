{-# LANGUAGE DeriveFunctor, FlexibleInstances, GeneralizedNewtypeDeriving #-}

import Text.Printf

import Control.Applicative

data Eqn a = Eqn {eqnRep :: String, runEqn :: a}
  deriving (Eq, Show, Ord, Functor)

instance Applicative Eqn where
  pure x = Eqn "" x
  (Eqn fs f) <*> (Eqn xs x) = Eqn (printf "%s(%s)" fs xs) (f x)


instance Num a => Num (Eqn a) where
  Eqn sa a + Eqn sb b = Eqn (printf "%s+%s" sa sb) (a+b)
  Eqn sa a - Eqn sb b = Eqn (printf "%s-%s" sa sb) (a-b)
  Eqn sa a * Eqn sb b = Eqn (printf "%s*%s" sa sb) (a*b)  
  abs (Eqn sa a) = Eqn (printf "|%s|" sa) (abs a)
  signum (Eqn sa a) = Eqn (printf "sgn(%s)" sa) (signum a)  
  negate (Eqn sa a) = Eqn (printf "-%s" sa) (negate a)  
  fromInteger n = Eqn (show n) (fromInteger n)
  
  
paren :: Eqn a -> Eqn a
paren (Eqn sa a) = Eqn (printf "(%s)" sa) a
         
instance Fractional a => Fractional (Eqn a) where
  fromRational n = Eqn (show n) (fromRational n)  
  Eqn sa a / Eqn sb b = Eqn (printf "%s/%s" sa sb) (a/b)    
  
solarMass :: Eqn Double  
solarMass = Eqn "Msun" 2e33

solarSpeed :: Eqn Double  
solarSpeed = Eqn "Vsun" 1.5e8
  
kinNrgF :: (Fractional a) => Eqn a -> Eqn a -> Eqn a  
kinNrgF m v = 0.5 * m * v ^ 2

kinNrg :: (Fractional a) => a -> a -> a
kinNrg m v = runEqn $ kinNrgF (pure m) (pure v)


solarNrg = kinNrgF solarMass solarSpeed

main :: IO ()  
main = do
  putStrLn $ "Kinetic energy is: " ++ (eqnRep $ solarNrg) ++ "\t(1)"
  putStrLn $ "Therefore, kinetic energy of the sun is: "
  print $ runEqn $ solarNrg
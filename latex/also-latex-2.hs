{-# LANGUAGE FlexibleInstances, GeneralizedNewtypeDeriving #-}

import Text.Printf

class Formula a where

newtype Eqn = Eqn String
  
newtype F a = F a  


           
instance Num a => Num (Eqn a) where
  Eqn sa a + Eqn sb b = Eqn (printf "%s+%s" sa sb) (a+b)
  Eqn sa a - Eqn sb b = Eqn (printf "%s-%s" sa sb) (a-b)
  Eqn sa a * Eqn sb b = Eqn (printf "%s*%s" sa sb) (a*b)  
  abs (Eqn sa a) = Eqn (printf "|%s|" sa) (abs a)
  signum (Eqn sa a) = Eqn (printf "sgn(%s)" sa) (signum a)  
  negate (Eqn sa a) = Eqn (printf "-%s" sa) (negate a)  
  fromInteger n = Eqn (show n) (fromInteger n)
  
  
paren :: (Formula a) => a -> a
  
instance Fractional a => Fractional (Eqn a) where
  fromRational n = Eqn (show n) (fromRational n)  
  Eqn sa a / Eqn sb b = Eqn (printf "%s/%s" sa sb) (a/b)    
  
solarMass :: Eqn Double  
solarMass = Eqn "Msun" 2e33

solarSpeed :: Eqn Double  
solarSpeed = Eqn "Vsun" 1.5e8
  
kinNrgF :: Eqn Double -> Eqn Double -> Eqn Double  
kinNrgF m v = 0.5 * m * v^2 



solarNrg = kinNrgF solarMass solarSpeed

main :: IO ()  
main = do
  print $ solarNrg
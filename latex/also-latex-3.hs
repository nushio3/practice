{-# LANGUAGE DeriveFunctor, FlexibleInstances, GeneralizedNewtypeDeriving #-}

import Text.Printf

import Control.Applicative

data Eqn r a = Eqn {eqnRep :: r, eqnVal :: a}
  deriving (Eq, Show, Ord, Functor)


instance Num a => Num (Eqn String a) where
  Eqn sa a + Eqn sb b = Eqn (printf "%s+%s" sa sb) (a+b)
  Eqn sa a - Eqn sb b = Eqn (printf "%s-%s" sa sb) (a-b)
  Eqn sa a * Eqn sb b = Eqn (printf "%s*%s" sa sb) (a*b)  
  abs (Eqn sa a) = Eqn (printf "|%s|" sa) (abs a)
  signum (Eqn sa a) = Eqn (printf "sgn(%s)" sa) (signum a)  
  negate (Eqn sa a) = Eqn (printf "-%s" sa) (negate a)  
  fromInteger n = Eqn (show n) (fromInteger n)
  
  

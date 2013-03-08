{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}

import Prelude hiding (empty, lookup)

infixr 6 :+
infixl 9 :>

data k :> v = k :> v deriving (Eq, Show)
data k :=> v = Nil
             | (k :> v) :+ (k :=> v)
             deriving (Eq, Show)

class a :->! b where
  (->!) :: a -> b -> b

empty :: k :=> v
empty = Nil

lookup :: k :=> v -> k -> Maybe v
lookup = undefined

instance (Eq k) => k :->! (k :=> v) where
  x ->! Nil = Nil
  x ->! ((x' :> y') :+ tail)
    |  x == x'   = x ->! tail
    |  otherwise = (x' :> y') :+ (x ->! tail)
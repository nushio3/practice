{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

import Control.Applicative
import Control.Monad

newtype W f a = W {un :: f a}

instance (Functor f) => Functor (W f) where
  fmap func (W xs) = W (fmap func xs)

instance (Applicative f, Functor (W f)) => Monad (W f) where
  return  = W . pure
  -- m >>= g = (\(W x) -> x) ((fmap g) m)
main = print "hi"

joinW :: (Monad (W f)) => W f (W f a) -> W f a
joinW = join
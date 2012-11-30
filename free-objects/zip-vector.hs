{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}

import           Control.Applicative
import           Data.Key
import qualified Data.Vector as V
import Prelude hiding (zipWith)


instance Zip V.Vector where
  zipWith = V.zipWith

data W f a
  = Scalar a
  | Array (f a) deriving (Eq, Show)

instance (Functor f) => Functor (W f) where
  fmap f (Scalar x) = Scalar (f x)
  fmap f (Array xs) = Array (fmap f xs)

instance (Functor f, Zip f, Functor (W f)) => Applicative (W f) where
  pure = Scalar
  (Scalar f) <*> (Scalar x) = Scalar (f x)
  (Scalar f) <*> (Array xs) = Array (fmap f xs)
  (Array fs) <*> (Scalar x) = Array (fmap ($x) fs)
  (Array fs) <*> (Array xs) = Array (zipWith ($) fs xs)

xs, ys, zs, ans :: W V.Vector Int
xs = Array $ V.fromList [1,2,3]
ys = Array $ V.fromList [4,5,6]
zs = Array $ V.fromList [7,8,9]

ans = liftA3 go xs ys zs where
  go x y z = 2 * (negate y - 3 * x) + 8 * z


backend :: (f a -> f a) -> W f a -> W f a
backend func (Array xs) = Array (func xs)
backend func x          = x

main = do
  print $ ans
  print $ liftA2 go (backend V.reverse xs) ys where
     go x y = x * y
  print $ liftA2 go (backend V.reverse xs) ys where
     go x y = x * y

{-# LANGUAGE FlexibleInstances #-}
import           Control.Monad
import           Control.Monad.Free
import qualified Data.Array.Repa as R
import qualified Data.Array.Repa.Eval as R

instance Functor (R.Array R.D R.DIM1) where
  fmap = R.map

xs, ys, zs :: Free (R.Array R.D R.DIM1) Int
xs = liftF $ R.delay $ R.fromListUnboxed (R.ix1 10) [1..10]

extract :: (Functor f) => Free f a -> f a
extract (Free xs) = fmap (\(Pure x) -> x) xs
extract _ = undefined

ys = Pure 2

zs = do
  x <- xs
  y <- ys
  return $ 10 * x + y

main = do
  rzs <- R.computeUnboxedP $ extract zs
  print $ R.toList rzs
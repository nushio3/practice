{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}

{-# OPTIONS -fno-warn-wrong-do-bind #-}

import ZipN

import           Control.Monad
import qualified Data.Array.Repa as R
import qualified Data.Array.Repa.Eval as R


instance (R.Shape sh) => Functor (R.Array R.D sh) where
  fmap = R.map

instance (R.Shape sh) => Zip2 (R.Array R.D sh) where
  zip2 = R.zipWith (,)

type instance IsFunction (R.Array R.D sh) = HFalse

xs, ys, zs :: R.Array R.D R.DIM1 Int
xs = R.delay $ R.fromListUnboxed (R.ix1 3) [1,2,3]
ys = R.delay $ R.fromListUnboxed (R.ix1 3) [10,11,12]
zs = R.delay $ R.fromListUnboxed (R.ix1 3) [100,110,120]

test1 = zipN xs (\x -> (x::Int) + 1)
-- [2,3,4]

test2 = zipN xs ys (\x y -> ((x::Int) + y))
-- [11,13,15]

test3 = zipN xs ys zs (\x y z -> ((x::Int) + y + z))
-- [111,123,135]

main = do
  computeAndPrint test1
  computeAndPrint test2
  computeAndPrint test3
    where
      computeAndPrint = R.computeUnboxedP >=> print

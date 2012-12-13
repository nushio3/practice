{-# LANGUAGE TypeFamilies #-}

import ZipN

import qualified Data.Vector as V

instance Zip2 V.Vector where
    zip2 = V.zip

type instance IsFunction V.Vector = HFalse

xs, ys, zs :: V.Vector Int
xs = V.fromList [1,2,3]
ys = V.fromList [10,11,12]
zs = V.fromList [100,110,120]

test1 = zipN xs (\x -> (x::Int) + 1)
-- [2,3,4]

test2 = zipN xs ys (\x y -> ((x::Int) + y))
-- [11,13,15]

test3 = zipN xs ys zs (\x y z -> ((x::Int) + y + z))
-- [111,123,135]

main = do
  print test1
  print test2
  print test3
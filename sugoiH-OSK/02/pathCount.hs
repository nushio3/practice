{-# LANGUAGE FlexibleContexts #-}

import Control.Monad.Memo

fibSlow :: Integer -> Integer
fibSlow 1 = 1
fibSlow 2 = 1
fibSlow n = fibSlow (n-1) + fibSlow (n-2)

fib = 1:1:  zipWith (+) fib (tail fib)


pcSlow :: Int -> Int -> Integer
pcSlow 0 _ = 1
pcSlow _ 0 = 1
pcSlow x y = pcSlow (x - 1) y + pcSlow x (y - 1)

pcList :: [[Integer]]
pcList = repeat 1 :
         map gen pcList
  where
    gen xs = let ys = 1 : zipWith (+) (tail xs) ys in ys

pcM :: MonadMemo (Int,Int) Integer m => (Int,Int) -> m Integer
pcM (0,_) = return 1
pcM (_,0) = return 1
pcM (x,y) = do
  a <- memo pcM (x-1, y)
  b <- memo pcM (x, y-1)
  return $ a+b

pc x y = startEvalMemo $ pcM (x,y)
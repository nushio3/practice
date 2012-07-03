{-# OPTIONS -Wall #-}
module Fib  where

-- | Memoized computation of Fibonacci numbers
--
-- Examples:
--
-- >>> fib 10
-- 55
--
-- >>> print $ show $ fib 5
-- "5"


fib :: Int -> Integer
fib = (xs!!)
  where
    xs = 0:1:(zipWith (+) xs (tail xs))


-- | The definition of Fibonacci numbers
--    
-- >>> fib' 100
-- 354224848179261915075
--
-- The original definition and memoized computation
-- of Fibonacci numbers should disagree
-- (,which is a wrong property!)
--
-- prop> \n -> fib' n /= fib n

fib' :: Int -> Integer
fib' 0 = 0
fib' 1 = 1
fib' n = fib (n - 1) + fib (n - 2)


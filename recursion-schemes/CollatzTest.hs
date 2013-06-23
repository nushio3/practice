module Main where

import Test.QuickCheck.Arbitrary (Arbitrary(..))
import Test.Hspec
import Test.Hspec.QuickCheck (prop)



cata :: b -> (a -> b -> b) -> [a] -> b
cata b f = h where
  h []     = b
  h (x:xs) = x `f` h xs

ana :: (b -> (a, b)) -> (b -> Bool) -> b -> [a]
ana g p = h where
  h b
    | p b       = []
    | otherwise = a : h b' where (a, b') = g b


collatz :: Integer -> Integer
collatz n
  | n <= 1    = 0
  | even n    = 1 + collatz (div n 2)
  | otherwise = 1 + collatz (3*n + 1)

collatzCPS :: Integer -> (Integer -> a) -> a
collatzCPS n k
  | n <= 1    = k 0
  | even n    = collatzCPS (div n 2) $ \ret -> k (ret+1)
  | otherwise = collatzCPS (3*n + 1) $ \ret -> k (ret+1)

collatzWithCPS :: Integer -> Integer
collatzWithCPS n = collatzCPS n id


data Step a
    = Yield a
    | Continue Integer (Integer -> Step a)

collatzGo :: Integer -> (Integer -> a) -> Step a
collatzGo n k
  | n <= 1    = Yield $ k 0
  | even n    = Continue (div n 2) $ \ret -> Yield $ k (ret+1)
  | otherwise = Continue (3*n + 1) $ \ret -> Yield $ k (ret+1)





main :: IO ()
main = hspec $ do
  describe "collatz in cps" $ do
    prop "matches the original collatz" $ \n ->
      collatz n == collatzWithCPS n

module Main where

import Test.QuickCheck.Arbitrary (Arbitrary(..))
import Test.Hspec
import Test.Hspec.QuickCheck (prop)

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


-- data CollatzStream a = Done a
--                  | StillWorking Integer (Integer -> a)



main :: IO ()
main = hspec $ do
  describe "collatz in cps" $ do
    prop "matches the original collatz" $ \n ->
      collatz n == collatzWithCPS n

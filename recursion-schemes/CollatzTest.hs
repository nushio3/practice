module Main where

import Test.QuickCheck.Arbitrary (Arbitrary(..))
import Test.Hspec
import Test.Hspec.QuickCheck (prop)

cata :: b -> (a -> b -> b) -> [a] -> b
cata b f = h where
  h []     = b
  h (x:xs) = x `f` h xs

ana :: (b -> Maybe (a, b)) -> b -> [a]
ana g = h where
  h b = case g b of
    Nothing       -> []
    Just (a', b') -> a' : h b'

fix :: (a -> a)-> a
fix = cata undefined ($) . ana (\f -> Just (f,f))



collatz :: Integer -> Integer
collatz n
  | n <= 1    = 0
  | even n    = 1 + collatz (div n 2)
  | otherwise = 1 + collatz (3*n + 1)

collatzWithFix :: Integer -> Integer
collatzWithFix = fix go
  where
    go :: (Integer -> Integer) -> Integer -> Integer
    go colla n
      | n <= 1    = 0
      | even n    = 1 + colla (div n 2)
      | otherwise = 1 + colla (3*n + 1)



main :: IO ()
main = hspec $ do
  describe "collatz in cps" $ do
    prop "matches the original collatz" $ \n ->
      collatz n == collatzWithFix n

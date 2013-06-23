module Main where


-- http://hackage.haskell.org/package/recursion-schemes
import qualified Data.Functor.Foldable as RS
import Test.QuickCheck.Arbitrary (Arbitrary(..))
import Test.Hspec
import Test.Hspec.QuickCheck (prop)

-- Straightfoward definition of fibonacci numbers

fib :: Integer -> Integer
fib n
  | n <= 1    = 1
  | otherwise = fib (n-1) + fib (n-2)

-- Memoized fib using list (it's fast!)

fibL :: [Integer]
fibL = 1:1: zipWith (+) fibL (tail fibL)

fibWithL :: Integer -> Integer
fibWithL n = fibL !! fromIntegral n

-- fib in continuation passing style.

fibCPS :: Integer -> (Integer -> a) -> a
fibCPS n k
  | n <= 1    = k 1
  | otherwise = fibCPS (n-1) $
                \val1 -> fibCPS (n-2) $
                         \val2 -> k (val1 + val2)

fibWithCPS :: Integer -> Integer
fibWithCPS n = fibCPS n id

-- fib defined using the recursion primitives.
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


fibAbs :: (Integer -> Integer) -> Integer -> Integer
fibAbs fib n
  | n <= 1    = 1
  | otherwise = fib (n-1) + fib (n-2)

fibWithFix :: Integer -> Integer
fibWithFix = fix fibAbs


-- fix using recursion scheme definitions
fix' :: (a -> a) -> a
fix' = RS.hylo (\(RS.Cons f x) -> f x)
               (\f -> RS.Cons f f)

fibWithFix' :: Integer -> Integer
fibWithFix' = fix' fibAbs


-- utilitiy for testing
newtype Small = Small Integer deriving (Show)
instance Arbitrary Small where
  arbitrary = fmap (Small . (`mod` 32)) arbitrary
  shrink = const []



-- main test routine

main :: IO ()
main = hspec $ do
  describe "fibonacci" $ do
    prop "cps style matches the original fib" $ \(Small n) ->
      fib n == fibWithCPS n
    prop "list style matches the original fib" $ \(Small n) ->
      fib n == fibWithL n
    prop "fix style matches the original fib" $ \(Small n) ->
      fib n == fibWithFix n
    prop "recursion-scheme version matches the original fib" $ \(Small n) ->
      fib n == fibWithFix' n

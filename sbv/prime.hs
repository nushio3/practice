{-# LANGUAGE ScopedTypeVariables #-}

import Control.Monad
import Data.SBV

isPrime :: SInteger -> Predicate
isPrime n = forAll ["k"] $ \k ->
  (2 .<= k &&& k .< n) ==> n `sMod` k ./= 0

infinitePrimeTheorem :: Predicate
infinitePrimeTheorem =
  forAll ["n"] $ \n ->
    forSome ["p"] $ \p -> do
      constrain $ p .>= n
      isPrime p


main = do
  (print =<<) $ prove $ isPrime 2
  (print =<<) $ prove $ isPrime 3
  (print =<<) $ prove $ isPrime 4
  (print =<<) $ prove $ isPrime 5
  (print =<<) $ prove $ isPrime 6

  putStrLn "are there infinite primes?"
  (print =<<) $ prove $ infinitePrimeTheorem

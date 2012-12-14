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
      ip <- isPrime p
      return $ ip &&& (p .>= n)


infiniteEvenPrimeTheorem :: Predicate
infiniteEvenPrimeTheorem =
  forAll ["n"] $ \n ->
    forSome ["p"] $ \p -> do
      ip <- isPrime p
      return $ ip &&& (p .>= n) &&& (p `sMod` 2 .== 0)

eulerLucky :: SInteger -> Predicate
eulerLucky a = forAll ["n"] $ \n -> do
  constrain $ (0 .<= n &&& n .<= a-2)
  isPrime $ n^2+n+a

main = do
  putStrLn "is there a prime larger than 100?"
  (print =<<) $ sat $ forSome ["p"] $ \p-> do
    constrain $ p .> 100
    isPrime p

  putStrLn "is there an even prime larger than 2?"
  (print =<<) $ sat $ forSome ["p"] $ \p-> do
    constrain $ p .> 2
    constrain $ p `sMod` 2 .== 0
    isPrime p


  putStrLn "are there infinite even primes?"
  (print =<<) $ prove $ infiniteEvenPrimeTheorem

  putStrLn "are there infinite primes?"
  (print =<<) $ prove $ infinitePrimeTheorem


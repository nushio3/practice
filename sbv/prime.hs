{-# LANGUAGE ScopedTypeVariables #-}

import Control.Monad
import Data.SBV

isPrime :: SInteger -> SBool
isPrime n = forAll_ $ \k ->
  (2 .<= k .&& k .< n) ==> n `sMod` k ./= 0

main = do
  (print =<<) $ prove $ isPrime 2
  (print =<<) $ prove $ isPrime 3
  (print =<<) $ prove $ isPrime 4
  (print =<<) $ prove $ isPrime 5
  (print =<<) $ prove $ isPrime 6

{-# LANGUAGE ScopedTypeVariables #-}

import Data.SBV

main = do
  sol <- minimize Quantified sum 3 (bAll (.>= (10 :: SReal)))
  print sol
  sol <- minimize Quantified sum 3 (bAll (.> (10 :: SReal)))
  print sol

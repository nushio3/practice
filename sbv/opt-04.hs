{-# LANGUAGE ScopedTypeVariables #-}

import Data.SBV

main = do
  sol <- minimize (Iterative True) costFunction 3 constraintFunction
  print sol
  where
    costFunction [x,y,z] = 42 * x + 10 * y + 8 * (z::SReal)
                           + z^2/10
    constraintFunction [x,y,z] = bAnd
      [ x .>= 0,
        y .>= 0,
        z .>= 0,
        x+y+z .>= 1500,
        y .>= x+z,
        x .>= 300
      ]
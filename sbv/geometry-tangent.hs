{-# LANGUAGE ScopedTypeVariables #-}

import Control.Monad
import Data.SBV

main = do
  (print =<<) $ minimize Quantified costf 3 constr
  where
    costf  [x,y,a] = (a::SReal)
    constr [x,y,a] = bAnd [y .== a*x-1, y .== x^4,  a .>= 0]

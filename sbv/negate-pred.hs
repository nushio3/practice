{-# LANGUAGE ScopedTypeVariables #-}

import Control.Monad
import Data.SBV

main = do
  let pred = do
       x <- sInt8 "x"
       constrain $ x .>= 120
       return (x .<= 124)

  (print =<<) $ allSat pred

  -- @fmap bnot@ does not negate the entire pred;
  -- it only negates the final expression
  -- but doesn't negate the constraints.
  (print =<<) $ allSat $ fmap bnot pred

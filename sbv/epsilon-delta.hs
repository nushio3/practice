{-# LANGUAGE ScopedTypeVariables #-}

import Data.SBV

main = do
   res <- prove $ do
     x <- forall "x"
     constrain $ x .>= 0.1
     constrain $ x .<= 3
     return $ 1/(x::SReal)^2 .<= 2
   print res

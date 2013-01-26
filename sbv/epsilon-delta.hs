{-# LANGUAGE ScopedTypeVariables #-}

import Data.SBV

main = do
   res <- prove $ do
     x <- forall "x"
     y <- exists "y"
     return $ y .>= (x::SReal)
   print res

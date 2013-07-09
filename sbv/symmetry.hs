{-# LANGUAGE ScopedTypeVariables #-}

import Data.SBV

main = do
   res <- prove $ \(x::SReal) y z -> (x + y) * z .== z * y + x * z
   print res
   res <- prove $ \(x::SReal) y z -> (x + y) * z ./= z * y + x * (z + 3)
   print res
   res <- prove $ \(x::SInteger) y z -> x*x*x + y*y*y ./= z*z*z
   print res

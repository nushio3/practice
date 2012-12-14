{-# LANGUAGE ScopedTypeVariables #-}

import Data.SBV

main = do
   res <- prove $ \(x::SWord8) -> x `shiftL` 2 .== 4*x
   print res

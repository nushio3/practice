{-# LANGUAGE ScopedTypeVariables #-}

import Data.SBV

main = do
   res <- prove $ forSome ["x"] $ \(x::SInteger) -> x * x .== 4
   print res
   res <- prove $ forSome ["x"] $ \(x::SReal) -> x * x .== 4
   print res
   res <- prove $ forSome ["x"] $ \(x::SInteger) -> x * x .== 2
   print res
   res <- prove $ forSome ["x"] $ \(x::SReal) -> x * x .== 2
   print res

{-# LANGUAGE ScopedTypeVariables #-}

import Control.Monad
import Data.SBV

exercise prob = do
  print =<< prove prob          
  print =<< sat prob          
  putStrLn ""

main = do
  exercise $ forSome ["x"] $ \(x::SInteger) -> x * x .== 4
  exercise $ forSome ["x"] $ \(x::SReal) -> x * x .== 4
  exercise $ forSome ["x"] $ \(x::SInteger) -> x * x .== 2
  exercise $ forSome ["x"] $ \(x::SReal) -> x * x .== 2


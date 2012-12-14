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

{-
$ runhaskell square-root.hs
Q.E.D.
Satisfiable. Model:
  x = 2 :: SInteger

Unknown
Satisfiable. Model:
  x = 2.0 :: SReal

Falsifiable
Unsatisfiable

Unknown
Satisfiable. Model:
  x = root(1, x^2 = 2) = -1.414213562373095... :: SReal
-}
{-# LANGUAGE ScopedTypeVariables #-}

import Control.Monad
import Data.SBV

exercise prob = do
  print =<< prove prob          
  print =<< sat prob          
  putStrLn ""

main = do
  exercise $ forSome ["x"] $ \(x::SReal) -> sin x .== 2



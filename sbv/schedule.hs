{-# LANGUAGE ScopedTypeVariables #-}

import Data.SBV

main = 
  (print =<<) $ sat $ do
    x <- exists "x"
    return $ x .>= (10 :: SReal)

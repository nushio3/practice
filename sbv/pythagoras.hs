{-# LANGUAGE ScopedTypeVariables #-}

import Control.Monad
import Data.SBV

pytha :: SInteger -> SInteger -> SInteger -> SBool
pytha a b c =
  (a .< b) &&& (b .< c) &&& (a^2 + b^2 .== c^2)

main = do
  (print =<<) $ sat $ forSome ["p","q","r"] $ \p q r-> do
    constrain $ p .>= 100
    return $ pytha p q r

  (print =<<) $ prove $
    forAll ["n"] $ \n -> do
      forSome ["p", "q","r"] $ \p q r-> do
        (p .>= n &&& pytha p q r)
{-# LANGUAGE ScopedTypeVariables #-}

import Data.SBV

isCommonDivisor :: SInteger -> SInteger -> SInteger -> SBool
isCommonDivisor a b k =
  a `sMod` k .== 0 &&&
  b `sMod` k .== 0

rgcd :: SInteger -> SInteger -> SInteger -> Symbolic SBool
rgcd a b k = do
  constrain $ isCommonDivisor a b k
  forAll_ $ \(k' :: SInteger) -> do
    isCommonDivisor a b k' ==> k' .<= k

main = do
  ans <- sat $ forSome_ $ \k -> rgcd 123456789 987654321 k
  print ans

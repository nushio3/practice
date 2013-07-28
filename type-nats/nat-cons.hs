{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeOperators #-}
import GHC.TypeLits

data a ^+ (n :: Nat)
data a ^- (n :: Nat)

type Rock = Int ^- 3

main = do
  print "hi"
  
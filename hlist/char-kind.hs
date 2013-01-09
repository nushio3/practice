{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
import  GHC.TypeLits
data Chara = A | B | C

data Proxy (a :: Chara) = Proxy

x :: Proxy A
x = Proxy

data ProxyB (a :: Bool) = ProxyB

y :: ProxyB False
y = ProxyB


data ProxyC (a :: Nat) = ProxyC

z :: ProxyC 3
z = ProxyC


main = do
  print "hi"
{-# LANGUAGE RankNTypes, TypeSynonymInstances, RebindableSyntax, ScopedTypeVariables #-}

import Prelude hiding (return, (>>=), (=<<))

data A
data B

type Not a = a -> B
type NN a = (a -> B) -> B

callCC :: forall a .((a -> NN B) -> NN a) -> NN a
callCC f = \ c -> (f (\ x -> \ _ -> c x)) c

exmid :: NN (Either A (A -> NN B) )
exmid = callCC f
  where
    f k = \g -> g (Right (\x -> k (Left x)))




type Stone = A
type GodPower = B
type ScrollOf a = a -> GodPower
type Greater a = NN a

proof_ggg_eq_g :: Greater GodPower -> GodPower
proof_ggg_eq_g f = f id

cccA :: forall a. ((a -> Greater GodPower) -> Greater a) -> Greater a
cccA = callCC

response :: Greater (Either Stone (Stone -> Greater GodPower))
response = exmid



main :: IO ()
main = print "hi"

{-# LANGUAGE RankNTypes, TypeSynonymInstances, RebindableSyntax #-}

import Prelude hiding (return, (>>=), (=<<))


data UmaiBo



type M a = (a -> UmaiBo) -> UmaiBo

return :: a -> M a
return x = ($x)


infixl 1 =<<
(>>=) :: M a -> (a -> M b) -> M b
m >>= k  = \ c -> m (\ x -> (k x) c)

infixr 1 >>=
k =<< m = m >>= k


{-

callcc (
   fun (k: (a, a->⊥) sum -> ⊥) ->
         InRight (fun (x:a) -> k (InLeft x)))
)
何をやってるかわからねーですが、要は、いったん "not (aまたはnot a) ならば (aまたはnot a)" という型のつくプログラム（これは頑張れば作れます）を作って、それに callccを適用しておしまい、という寸法でした。

-}



midi :: M (Either (a -> M b) a)
midi = callCC (3::Int)


exmid :: M (Either (a -> M b) a)
exmid = exeither' exmid'
  where
    exmid' f g = either return g =<< callCC
                 (\cc -> return . Left =<< f (cc . Right))
    exeither' e = e (return . Left) (return . Right)


callCC :: ((a -> M b) -> M a) -> M a
callCC f =
  \ c -> (f (\ x -> \ _ -> c x)) c


data Stone
data Gold

resp :: M (Either (Stone -> M Gold) Stone)
resp = exmid


{-

-- "not (aまたはnot a) ならば (aまたはnot a)
type Not a = a -> String

ccc :: ((a -> b) -> a) -> a
ccc = undefined

hyper :: Not (Either a   (Not a )) -> (Either a  (Not a ))
hyper = undefined

super :: Either a  (Not a )
super = ccc hyper
-}
{-
type N a = forall r. (a -> r) -> r
callC2 :: ((a -> N b) -> N a) -> N a
callC2 f = \c -> f (\x -> (\_ -> c x)) c
-}

main :: IO ()
main = print "hi"

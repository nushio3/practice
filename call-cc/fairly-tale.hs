{-# LANGUAGE RankNTypes, TypeSynonymInstances #-}

import Control.Applicative
import Control.Monad

data UmaiBo


data M a = M { runM :: (a -> UmaiBo) -> UmaiBo}

instance Functor M where
  fmap = liftM

instance Applicative M where
  pure = return
  (<*>) = ap

instance Monad M where
  return x = M ($x)
  m >>= k  = M $ \ c -> runM m (\ x -> runM (k x) c)

exmid :: M (Either (a -> M b) a)
exmid = exeither' exmid'
     where
       exmid' f g = either return g =<< callCC (\cc ->
                              return . Left =<< f (cc . Right))
       exeither' e = e (return . Left) (return . Right)






callCC :: ((a -> M b) -> M a) -> M a
callCC f =
  M $ \ c -> runM (f (\ x -> M $ \ _ -> c x)) c


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

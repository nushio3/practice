{-# LANGUAGE RankNTypes, TypeSynonymInstances, ScopedTypeVariables #-}

{-

Wadler. http://homepages.inf.ed.ac.uk/wadler/papers/dual/dual.pdf
Edward Yang. http://blog.ezyang.com/2013/04/a-classical-logic-fairy-tale/
JRF.   http://jrf.cocolog-nifty.com/column/2011/01/post-1.html
d.y.d. http://www.kmonos.net/wlog/61.html#_0538060508

  -}

import Control.Applicative
import Control.Monad

data B
data A

instance Functor NN where
  fmap = liftM

instance Applicative NN where
  pure = return -- :: a -> NN a
  (<*>) = ap    -- :: NN (a -> b) -> NN a -> NN b

unap :: (NN a -> NN b) -> NN (a -> b)
unap f = undefined -- impossible, it seems.


instance Monad NN where
    return x = NN ($ x)
    m >>= k  = NN $ \ c -> runNN m (\ x -> runNN (k x) c)


data NN a = NN {runNN :: (a -> B) -> B}



exmid :: NN (Either (a -> NN B) a)
exmid = exeither' exmid'
  where
    exmid' f g = either return g =<< callCC (\cc ->
                           return . Left =<< f (cc . Right))
    exeither' e = e (return . Left) (return . Right)



{-

)
-}

-- midi :: forall a. NN (Either a (a -> NN B))
-- midi = callCC <*> f
--   where
--     f :: (Either a (a -> NN B) -> NN B) -> NN (Either a (a -> NN B))
--     f = undefined

test :: NN Int -> NN Int
test = \x -> (*2) <$> x


callCC :: ((a -> NN B) -> NN a) -> NN a
callCC f = NN $ \ c -> runNN (f (\ x -> NN $ \ _ -> c x)) c


callCC2 :: ((A -> NN B) -> NN A) -> NN A
callCC2 f = NN $ \ c -> runNN
                        ((f :: (A -> NN B) -> NN A)
                         (\ x -> NN $ \ _ -> c (x :: A))) (c :: A -> B)



proof3 :: NN (A -> B) -> (A -> NN B)
proof3 nf a = nf <*> pure a

-- callCCA :: ((NN A -> NN B) -> NN A) -> NN A
-- callCCA f = NN $ \ c ->  runNN
--  ((f :: (NN A -> NN B) -> NN A)
--   (\ x -> \ _ -> c (x ::NN A))  :: NN A)
--      (c :: A -> B)


-- exmidA :: forall a. NN (Either a (a -> B))
-- exmidA = cccA <*> f
--   where f :: NN ((Either a (a -> B) -> B) -> Either a (a -> B))
--         f = undefined


--cccA :: ((NN a -> NN B) -> NN a) -> NN a
--cccA f = NN $ \ c -> runNN (f (\ x -> NN $ \ _ -> c x)) c


data Stone
type GodPower = B
type ScrollOf a = a -> GodPower
type Greater = NN




-- Greater Stone = (Stone -> GodPower) -> GodPower

cccA :: ((Stone -> Greater GodPower) -> Greater Stone) -> Greater Stone
cccA f = NN $ \ c -> runNN (f (\ x -> NN $ \ _ -> c x)) c


-- Either a (a -> GodPower)
ggg_eq_g :: Greater GodPower -> GodPower
ggg_eq_g f = runNN f id


resp :: Greater (Either (Stone -> Greater GodPower) Stone)
resp = exmid



exmid2 :: NN (Either A (A -> NN B) )
exmid2 = callCC f
  where
    f :: ((Either A (A -> NN B)) -> NN B) -> NN (Either A (A -> NN B))
    f (k :: (Either A (A -> NN B)) -> NN B) = return $
      (Right (\x -> k (Left x)))


{-
exmid
callcc (
   fun (k: (a, a->⊥) sum -> ⊥) ->
         InRight (fun (x:a) -> k (InLeft x)))
)


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

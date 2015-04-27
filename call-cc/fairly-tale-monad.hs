{-# LANGUAGE RankNTypes, TypeSynonymInstances, ScopedTypeVariables #-}

import Control.Applicative
import Control.Monad

data B
data A

instance Functor NN where
  fmap = liftM

instance Applicative NN where
  pure = return -- :: a -> NN a
  (<*>) = ap    -- :: NN (a -> b) -> NN a -> NN b

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

callcc (
   fun (k: (a, a->⊥) sum -> ⊥) ->
         InRight (fun (x:a) -> k (InLeft x)))
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

nn :: ((A -> B) -> B) -> NN A
nn = NN

callCC3 :: ((A -> NN B) -> NN A) -> NN A
callCC3 f = NN $ \ c ->  runNN
 ((f :: (A -> NN B) -> NN A)
  (\ x -> NN $ \ _ -> c (x :: A))  :: NN A)
     (c :: A -> B)


-- exmidA :: forall a. NN (Either a (a -> B))
-- exmidA = cccA <*> f
--   where f :: NN ((Either a (a -> B) -> B) -> Either a (a -> B))
--         f = undefined


--cccA :: ((NN a -> NN B) -> NN a) -> NN a
--cccA f = NN $ \ c -> runNN (f (\ x -> NN $ \ _ -> c x)) c


data Stone
type Life = B
type Scroll = Stone -> Life
type Greater = NN



cccA :: ((Stone -> NN B) -> Greater Stone) -> Greater Stone
cccA f = NN $ \ c -> runNN (f (\ x -> NN $ \ _ -> c x)) c



resp :: NN (Either (Stone -> Greater Life) Stone)
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

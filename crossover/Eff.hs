{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveDataTypeable, GeneralizedNewtypeDeriving, DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

-- The framework of extensible effects

module Eff where

import Control.Monad
import Data.Typeable
import OpenUnion1
-- import OpenUnion3

import Data.IORef                       -- For demonstration of lifting

-- A monadic library for communication between a handler and
-- its client, the administered computation

-- Status of a coroutine (client): done with the value of type w,
-- or sending a request of type Union r
data VE w r = Val w | E !(Union r (VE w r))

-- The Eff monad (not a transformer!)
-- It is actually
--     type Eff r = forall w. Cont (VE w r)
-- We inline it into Cont to put forall under newtype;
-- it is awkward otherwise in Haskell.
-- Also, in MTL, Cont is defined via transformers. We want to
-- avoid transformers!
newtype Eff r a = Eff{runEff :: forall w. (a -> VE w r) -> VE w r}

-- standard instances for a continuation monad
instance Functor (Eff r) where
    fmap f m = Eff $ \k -> runEff m (k . f)

instance Monad (Eff r) where
    {-# INLINE return #-}
    {-# INLINE (>>=) #-}
    return x = Eff $ \k -> k x
    m >>= f  = Eff $ \k -> runEff m (\v -> runEff (f v) k)

-- send a request and wait for a reply
send :: (forall w. (a -> VE w r) -> Union r (VE w r)) -> Eff r a
send f = Eff (E . f)

-- A specialization of send useful for explanation
send_req :: (Functor req, Typeable1 req, Member req r) =>
            (forall w. (a -> VE w r) -> req (VE w r)) -> Eff r a
send_req req = send (inj . req)

-- administer a client: launch a coroutine and wait for it
-- to send a request or terminate with a value
admin :: Eff r w -> VE w r
admin (Eff m) = m Val

-- The opposite of admin, occasionally useful
-- See the soft-cut for an example
-- It is currently quite inefficient. There are better ways
reflect :: VE a r -> Eff r a
reflect (Val x) = return x
reflect (E u) = Eff (\k -> E $ fmap (loop k) u)
 where
 loop :: (a -> VE w r) -> VE a r -> VE w r
 loop k (Val x) = k x
 loop k (E u)   = E $ fmap (loop k) u


-- ------------------------------------------------------------------------
-- The initial case, no effects

data Void -- no constructors

-- The type of run ensures that all effects must be handled:
-- only pure computations may be run.
run :: Eff Void w -> w
run m = case admin m of Val x -> x
-- the other case is unreachable since Void has no constructors
-- Therefore, run is a total function if m Val terminates.

-- A convenient pattern: given a request (open union), either
-- handle it or relay it.
handle_relay :: Typeable1 t =>
     Union (t :> r) v -> (v -> Eff r a) -> (t v -> Eff r a) -> Eff r a
handle_relay u loop h = case decomp u of
  Right x -> h x
  Left u  -> send (\k -> fmap k u) >>= loop
  -- perhaps more efficient:
  -- Left u  -> send (\k -> fmap (\w -> runEff (loop w) k) u)

-- Add something like Control.Exception.catches? It could be useful
-- for control with cut.

interpose :: (Typeable1 t, Functor t, Member t r) =>
     Union r v -> (v -> Eff r a) -> (t v -> Eff r a) -> Eff r a
interpose u loop h = case prj u of
  Just x -> h x
  _       -> send (\k -> fmap k u) >>= loop

-- ------------------------------------------------------------------------
-- The Reader monad

-- The request for a value of type e from the current environment
newtype Reader e v = Reader (e -> v)
    deriving (Typeable, Functor)

{--
instance Functor ((->) e) where
    fmap = (.)
--}

-- The signature is inferred
ask :: (Typeable e, Member (Reader e) r) => Eff r e
ask = send (inj . Reader)

-- The handler of Reader requests. The return type shows that
-- all Reader requests are fully handled.
runReader :: Typeable e => Eff (Reader e :> r) w -> e -> Eff r w
runReader m e = loop (admin m) where
 loop (Val x) = return x
 loop (E u) = handle_relay u loop (\(Reader k) -> loop (k e))

-- Locally rebind the value in the dynamic environment
-- This function is like a relay; it is both an admin for Reader requests,
-- and a requestor of them
local :: (Typeable e, Member (Reader e) r) =>
     (e -> e) -> Eff r a -> Eff r a
local f m = do
  e0 <- ask
  let e = f e0
  let loop (Val x) = return x
      loop (E u) = interpose u loop (\(Reader k) -> loop (k e))
  loop (admin m)

-- Examples
add :: Monad m => m Int -> m Int -> m Int
add = liftM2 (+)

-- The type is inferred
t1 :: Member (Reader Int) r => Eff r Int
t1 = ask `add` return (1::Int)

t1' :: Member (Reader Int) r => Eff r Int
t1' = do v <- ask; return (v + 1 :: Int)

-- t1r :: Eff r Int
t1r = runReader t1 (10::Int)

t1rr = run t1r
-- 11

{-
t1rr' = run t1
    No instance for (Member (Reader Int) Void)
      arising from a use of `t1'
-}

-- Inferred type
-- t2 :: (Member (Reader Int) r, Member (Reader Float) r) => Eff r Float
t2 = do
  v1 <- ask
  v2 <- ask
  return $ fromIntegral (v1 + (1::Int)) + (v2 + (2::Float))

-- t2r :: Member (Reader Float) r => Eff r Float
t2r = runReader t2 (10::Int)
-- t2rr :: Eff r Float
t2rr = flip runReader (20::Float) . flip runReader (10::Int) $ t2

t2rrr = run t2rr
-- 33.0

-- The opposite order of layers
{- If we mess up, we get an error
t2rrr' = run $ runReader (runReader (t2 ()) (20::Float)) (10::Float)
    No instance for (Member (Reader Int) Void)
      arising from a use of `t2'
-}
t2rrr' = run $ runReader (runReader t2 (20::Float)) (10::Int)
-- 33.0

-- The type is inferred
t3 :: Member (Reader Int) r => Eff r Int
t3 = t1 `add` local (+ (10::Int)) t1
t3r = run $ runReader t3 (100::Int)
-- 212

-- The following example demonstrates true interleaving of Reader Int
-- and Reader Float layers
{-
t4
  :: (Member (Reader Int) r, Member (Reader Float) r) =>
     () -> Eff r Float
-}
t4 = liftM2 (+) (local (+ (10::Int)) t2) 
                (local (+ (30::Float)) t2)

t4rr = run $ runReader (runReader t4 (10::Int)) (20::Float)
-- 106.0
-- The opposite order of layers gives the same result
t4rr' = run $ runReader (runReader t4 (20::Float)) (10::Int)
-- 106.0

-- Map an effectful function
-- The type is inferred
tmap :: Member (Reader Int) r => Eff r [Int]
tmap = mapM f [1..5]
 where f x = ask `add` return x

tmapr = run $ runReader tmap (10::Int)
-- [11,12,13,14,15]


-- ------------------------------------------------------------------------
-- Exceptions

-- exceptions of the type e; no resumption
newtype Exc e v = Exc e
    deriving (Functor, Typeable)

-- The type is inferred
throwError :: (Typeable e, Member (Exc e) r) => e -> Eff r a
throwError e = send (\_ -> inj $ Exc e)

runError :: Typeable e => Eff (Exc e :> r) a -> Eff r (Either e a)
runError m = loop (admin m)
 where 
 loop (Val x)  = return (Right x)
 loop (E u)    = handle_relay u loop (\(Exc e) -> return (Left e))

-- The handler is allowed to rethrow the exception
catchError :: (Typeable e, Member (Exc e) r) =>
        Eff r a -> (e -> Eff r a) -> Eff r a
catchError m handle = loop (admin m)
 where 
 loop (Val x)  = return x
 loop (E u)    = interpose u loop (\(Exc e) -> handle e)

-- The type is inferred
et1 :: Eff r Int
et1 = return 1 `add` return 2

et1r = run et1
-- 3

-- The type is inferred
et2 :: Member (Exc Int) r => Eff r Int
et2 = return 1 `add` throwError (2::Int)

-- The following won't type: unhandled exception!
-- ex2r = run et2
{-
    No instance for (Member (Exc Int) Void)
      arising from a use of `et2'
-}

-- The inferred type shows that ex21 is now pure
et21 :: Eff r (Either Int Int)
et21 = runError et2

et21r = run et21
-- Left 2

-- The example from the paper
newtype TooBig = TooBig Int deriving (Show, Typeable)
-- The type is inferred
ex2 :: Member (Exc TooBig) r => Eff r Int -> Eff r Int
ex2 m = do
  v <- m
  if v > 5 then throwError (TooBig v)
     else return v

-- specialization to tell the type of the exception
runErrBig :: Eff (Exc TooBig :> r) a -> Eff r (Either TooBig a)
runErrBig m = runError m

ex2r = runReader (runErrBig (ex2 ask)) (5::Int)

ex2rr = run ex2r
-- Right 5

ex2rr1 = run $ runReader (runErrBig (ex2 ask)) (7::Int)
-- Left (TooBig 7)

-- Different order of handlers (layers)
ex2rr2 = run $ runErrBig (runReader (ex2 ask) (7::Int))
-- Left (TooBig 7)

-- ------------------------------------------------------------------------
-- Non-determinism (choice)

-- choose lst non-deterministically chooses one value from the lst
-- choose [] thus corresponds to failure
data Choose v = forall a. Choose [a] (a -> v)
              deriving (Typeable)

instance Functor Choose where
    fmap f (Choose lst k) = Choose lst (f . k)

choose :: Member Choose r => [a] -> Eff r a
choose lst = send (\k -> inj $ Choose lst k)

-- MonadPlus-like operators are expressible via choose

mzero' :: Member Choose r => Eff r a
mzero' = choose []
mplus' m1 m2 = choose [m1,m2] >>= id


-- The interpreter
makeChoice :: forall a r. Eff (Choose :> r) a -> Eff r [a]
makeChoice m = loop (admin m)
 where
 loop (Val x)  = return [x]
 loop (E u)    = handle_relay u loop (\(Choose lst k) -> handle lst k)
 -- Need the signature since local bindings aren't polymorphic any more
 handle :: [t] -> (t -> VE a (Choose :> r)) -> Eff r [a]
 handle [] _  = return []
 handle [x] k = loop (k x)
 handle lst k = fmap concat $ mapM (loop . k) lst
 
exc1 :: Member Choose r => Eff r Int
exc1 = return 1 `add` choose [1,2]

exc11 = makeChoice exc1

exc11r = run exc11
-- [2,3]

-- ------------------------------------------------------------------------
-- Soft-cut: non-deterministic if-then-else, aka Prolog's *->
-- Declaratively,
--    ifte t th el = (t >>= th) `mplus` ((not t) >> el)
-- However, t is evaluated only once. In other words, ifte t th el
-- is equivalent to t >>= th if t has at least one solution.
-- If t fails, ifte t th el is the same as el.

ifte :: forall r a b.
        Member Choose r => Eff r a -> (a -> Eff r b) -> Eff r b -> Eff r b
ifte t th el = loop [] (admin t)
 where 
 loop [] (Val x)  = th x
 -- add all other latent choices of t to th x
 -- this is like reflection of t
 loop jq (Val x)  = choose ((th x) : map (\t -> reflect t >>= th) jq) >>= id 
 loop jq (E u)    = interpose u (loop jq) (\(Choose lst k) -> handle jq lst k)
 -- Need the signature since local bindings aren't polymorphic any more
 handle :: [VE a r] -> [t] -> (t -> VE a r) -> Eff r b
 handle [] [] _     = el                    -- no more choices left
 handle (j:jq) [] _ = loop jq j
 handle jq [x] k    = loop jq (k x)
 handle jq (x:rest) k = loop (map k rest ++ jq) (k x) -- DFS

guard' :: Member Choose r => Bool -> Eff r ()
guard' True  = return ()
guard' False = mzero'

-- primes (very inefficiently -- but a good example of ifte)
test_ifte = do
  n <- gen
  guard' $ n > 1
  ifte (do
     d <- gen
     guard' $ d < n && d > 1 && n `mod` d == 0
     -- _ <- trace ("d: " ++ show d) (return ())
     return d)
    (\_->mzero')
    (return n)
 where gen = choose [1..30]

test_ifte_run = run . makeChoice $ test_ifte
-- [2,3,5,7,11,13,17,19,23,29]

-- ------------------------------------------------------------------------
-- Combining exceptions and non-determinism

-- Example from the paper


ex2_2 = run . makeChoice . runErrBig $ ex2 (choose [5,7,1])
-- [Right 5,Left (TooBig 7),Right 1]

-- just like ex1_1 in transf.hs but not at all like ex2_1 in transf.hs

-- with different order of handlers, obtain the desired result of
-- a high-priority exception
ex2_1 = run . runErrBig . makeChoice $ ex2 (choose [5,7,1])
-- Left (TooBig 7)

-- Errror recovery part
-- The code is the same as in transf1.hs. The inferred signatures differ
-- Was: exRec :: MonadError TooBig m => m Int -> m Int
-- exRec :: Member (Exc TooBig) r => Eff r Int -> Eff r Int
exRec m = catchError m handler
 where handler (TooBig n) | n <= 7 = return n
       handler e = throwError e

ex2r_2 = run . runErrBig . makeChoice $ exRec (ex2 (choose [5,7,1]))
-- Right [5,7,1]
-- Compare with ex2r_1 from transf1.hs

ex2r_2' = run . makeChoice . runErrBig $ exRec (ex2 (choose [5,7,1]))
-- [Right 5,Right 7,Right 1]
-- Again, all three choices are accounted for.

ex2r_1 = run . runErrBig . makeChoice $ exRec (ex2 (choose [5,7,11,1]))
-- Left (TooBig 11)
-- Compare with ex2r_2 from transf1.hs

-- ------------------------------------------------------------------------
-- State, strict

data State s w = State (s->s) (s -> w)
  deriving (Typeable, Functor) 

-- The signature is inferred
put :: (Typeable s, Member (State s) r) => s -> Eff r ()
put s = send (\k -> inj (State (const s) (\_ -> k ())))

-- The signature is inferred
get :: (Typeable s, Member (State s) r) => Eff r s
get = send (\k -> inj (State id k))

runState :: Typeable s => Eff (State s :> r) w -> s -> Eff r (w,s)
runState m s = loop s (admin m) where
 loop s (Val x) = return (x,s)
 loop s (E u)   = handle_relay u (loop s) $
                       \(State t k) -> let s' = t s in s' `seq` loop s' (k s')

-- Examples

ts1 :: Member (State Int) r => Eff r Int
ts1 = do 
  put (10 ::Int)
  x <- get
  return (x::Int)

ts1r = run (runState ts1 (0::Int))
-- (10,10)

ts2 :: Member (State Int) r => Eff r Int
ts2 = do 
  put (10::Int)
  x <- get
  put (20::Int)
  y <- get
  return (x+y) 

ts2r = run (runState ts2 (0::Int))
-- (30,20)

-- exceptions and state
incr :: Member (State Int) r => Eff r ()
incr = get >>= put . (+ (1::Int))

tes1 :: (Member (State Int) r, Member (Exc [Char]) r) => Eff r b
tes1 = do
 incr
 throwError "exc"

ter1 :: (Either String String, Int)
ter1 = run $ runState (runError tes1) (1::Int)
-- (Left "exc",2)

ter2 :: Either String (String, Int)
ter2 = run $ runError (runState tes1 (1::Int))
-- Left "exc"


teCatch :: Member (Exc String) r => Eff r a -> Eff r [Char]
teCatch m = catchError (m >> return "done") (\e -> return (e::String))

ter3 :: (Either String String, Int)
ter3 = run $ runState (runError (teCatch tes1)) (1::Int)
-- (Right "exc",2)

ter4 :: Either String (String, Int)
ter4 = run $ runError (runState (teCatch tes1) (1::Int))
-- Right ("exc",2)


-- Encapsulation of effects
-- The example suggested by a reviewer

{- The reviewer outlined an MTL implementation below, writing
  ``This hides the state effect and I can layer another state effect on
  top without getting into conflict with the class system.''

class Monad m => MonadFresh m where
    fresh :: m Int

newtype FreshT m a = FreshT { unFreshT :: State Int m a }
      deriving (Functor, Monad, MonadTrans)

    instance Monad m => MonadFresh (FreshT m) where
      fresh = FreshT $ do n <- get; put (n+1); return n

See EncapsMTL.hs for the complete code.
-}

-- There are three possible implementations
-- The first one uses State Fresh where 
--    newtype Fresh = Fresh Int
-- We get the `private' effect layer (State Fresh) that does not interfere
-- with with other layers.
-- This is the easiest implementation.

-- The second implementation defines a new effect Fresh

newtype Fresh v = Fresh (Int -> v)
    deriving (Functor, Typeable)

fresh :: Member Fresh r => Eff r Int
fresh = send (inj . Fresh)

-- And a handler for it
runFresh' :: Eff (Fresh :> r) w -> Int -> Eff r w
runFresh' m s = loop s (admin m) where
 loop s (Val x) = return x
 loop s (E u)   = handle_relay u (loop s) $
                       \(Fresh k) -> (loop $! (s+1)) (k s)

-- Test
tfresh' = runTrace $ flip runFresh' 0 $ do
  n <- fresh
  trace $ "Fresh " ++ show n
  n <- fresh
  trace $ "Fresh " ++ show n

-- Finally, the worst implementation but the one that answers
-- reviewer's question: implementing Fresh in terms of State
-- but not revealing that fact.

runFresh :: Eff (Fresh :> r) w -> Int -> Eff r w
runFresh m s = runState m' s >>= return . fst
 where
 m' = loop (admin m)
 loop (Val x) = return x
 loop (E u)   = case decomp u of
  Right (Fresh k) -> do
                     n <- get
                     put (n+1::Int)
                     loop (k n)
  Left u  -> send (\k -> weaken $ fmap k u) >>= loop

tfresh = runTrace $ flip runFresh 0 $ do
  n <- fresh
  -- (x::Int) <- get
  trace $ "Fresh " ++ show n
  n <- fresh
  trace $ "Fresh " ++ show n

{-
If we try to meddle with the encapsulated state, by uncommenting the
get statement above, we get:
    No instance for (Member (State Int) Void)
      arising from a use of `get'
-}


-- ------------------------------------------------------------------------
-- Tracing (debug printing)

data Trace v = Trace String (() -> v)
    deriving (Typeable, Functor)

-- Printing a string in a trace
trace :: Member Trace r => String -> Eff r ()
trace x = send (inj . Trace x)

-- The handler for IO request: a terminal handler
runTrace :: Eff (Trace :> Void) w -> IO w
runTrace m = loop (admin m) where
 loop (Val x) = return x
 loop (E u)   = case prj u of
                  Just (Trace s k) -> putStrLn s >> loop (k ())
                  -- Nothing cannot occur

-- Higher-order effectful function
-- The inferred type shows that the Trace affect is added to the effects
-- of r
mapMdebug:: (Show a, Member Trace r) =>
     (a -> Eff r b) -> [a] -> Eff r [b]
mapMdebug f [] = return []
mapMdebug f (h:t) = do
 trace $ "mapMdebug: " ++ show h
 h' <- f h
 t' <- mapMdebug f t
 return (h':t')

tMd = runTrace $ runReader (mapMdebug f [1..5]) (10::Int)
 where f x = ask `add` return x
{-
mapMdebug: 1
mapMdebug: 2
mapMdebug: 3
mapMdebug: 4
mapMdebug: 5
[11,12,13,14,15]
-}

-- duplicate layers
tdup = runTrace $ runReader m (10::Int)
 where
 m = do
     runReader tr (20::Int)
     tr
 tr = do
      v <- ask
      trace $ "Asked: " ++ show (v::Int)
   
-- ------------------------------------------------------------------------
-- Lifting: emulating monad transformers

data Lift m v = forall a. Lift (m a) (a -> v)

-- For ST monad, we have to define LiftST since (ST s) can't be Typeable:
-- s must be polymorphic without any constraints

{--
ghci 7.6.3 ==>
Eff.hs:465:29: Warning:
    In the use of `mkTyCon' (imported from Data.Typeable):
    Deprecated: "either derive Typeable, or use mkTyCon3 instead"
--}
{-
instance Typeable1 m => Typeable1 (Lift m) where
    typeOf1 _ = 
     mkTyConApp (mkTyCon3 "" "Eff" "Lift") [typeOf1 (undefined:: m ())]
-}
deriving instance Typeable Lift

instance Functor (Lift m) where
    fmap f (Lift m k) = Lift m (f . k)

-- We make the Lift layer to be unique, using MemberU2
lift :: (Typeable1 m, MemberU2 Lift (Lift m) r) => m a -> Eff r a
lift m = send (inj . Lift m)

-- The handler of Lift requests. It is meant to be terminal
runLift :: (Monad m, Typeable1 m) => Eff (Lift m :> Void) w -> m w
runLift m = loop (admin m) where
 loop (Val x) = return x
 loop (E u)   = case prj u of
                  Just (Lift m k) -> m >>= loop . k
                  -- Nothing cannot occur

tl1 = ask >>= \(x::Int) -> lift . print $ x

-- tl1r :: IO ()
tl1r = runLift (runReader tl1 (5::Int))
-- 5

-- Re-implemenation of mapMdebug using Lifting
-- The signature is inferred
mapMdebug'  :: (Show a, MemberU2 Lift (Lift IO) r) =>
             (a -> Eff r b) -> [a] -> Eff r [b]
mapMdebug' f [] = return []
mapMdebug' f (h:t) = do
 lift $ print h
 h' <- f h
 t' <- mapMdebug' f t
 return (h':t')

tMd' = runLift $ runReader (mapMdebug' f [1..5]) (10::Int)
 where f x = ask `add` return x
{-
1
2
3
4
5
[11,12,13,14,15]
-}

 
-- ------------------------------------------------------------------------
-- Co-routines
-- The interface is intentionally chosen to be the same as in transf.hs

-- The yield request: reporting the value of type e and suspending 
-- the coroutine
-- (For simplicity, a co-routine reports a value but accepts unit)
data Yield a v = Yield a (() -> v)
    deriving (Typeable, Functor)

-- The signature is inferred
yield :: (Typeable a, Member (Yield a) r) => a -> Eff r ()
yield x = send (inj . Yield x)

-- Status of a thread: done or reporting the value of the type a
-- (For simplicity, a co-routine reports a value but accepts unit)
data Y r a = Done | Y a (() -> Eff r (Y r a))

-- Launch a thread and report its status
runC :: Typeable a => Eff (Yield a :> r) w -> Eff r (Y r a)
runC m = loop (admin m) where
 loop (Val x) = return Done
 loop (E u)   = handle_relay u loop $ 
                 \(Yield x k) -> return (Y x (loop . k))


-- First example of coroutines
th1 :: Member (Yield Int) r => Eff r ()
th1 = yield (1::Int) >> yield (2::Int)


c1 = runTrace (loop =<< runC th1)
 where loop (Y x k) = trace (show (x::Int)) >> k () >>= loop
       loop Done    = trace "Done"
{-
1
2
Done
-}

-- Add dynamic variables
-- The code is essentially the same as that in transf.hs (only added
-- a type specializtion on yield). The inferred signature is different though.
-- Before it was
--    th2 :: MonadReader Int m => CoT Int m ()
-- Now it is more general:
th2 :: (Member (Yield Int) r, Member (Reader Int) r) => Eff r ()
th2 = ask >>= yield' >> (ask >>= yield')
 where
 yield' x = yield (x::Int)


-- Code is essentially the same as in transf.hs; no liftIO though
c2 = runTrace $ runReader (loop =<< runC th2) (10::Int)
 where loop (Y x k) = trace (show (x::Int)) >> k () >>= loop
       loop Done    = trace "Done"
{-
10
10
Done
-}

-- locally changing the dynamic environment for the suspension
c21 = runTrace $ runReader (loop =<< runC th2) (10::Int)
 where loop (Y x k) = trace (show (x::Int)) >> local (+(1::Int)) (k ()) >>= loop
       loop Done    = trace "Done"
{-
10
11
Done
-}

-- Real example, with two sorts of local rebinding
th3 :: (Member (Yield Int) r, Member (Reader Int) r) => Eff r ()
th3 = ay >> ay >> local (+(10::Int)) (ay >> ay)
 where ay = ask >>= yield'
       yield' x = yield (x::Int)

c3 = runTrace $ runReader (loop =<< runC th3) (10::Int)
 where loop (Y x k) = trace (show (x::Int)) >> k () >>= loop
       loop Done    = trace "Done"
{-
10
10
20
20
Done
-}

-- locally changing the dynamic environment for the suspension
c31 = runTrace $ runReader (loop =<< runC th3) (10::Int)
 where loop (Y x k) = trace (show (x::Int)) >> local (+(1::Int)) (k ()) >>= loop
       loop Done    = trace "Done"
{-
10
11
21
21
Done
-}
-- The result is exactly as expected and desired: the coroutine shares the
-- dynamic environment with its parent; however, when the environment
-- is locally rebound, it becomes private to coroutine.

-- We now make explicit that the client computation, run by th4,
-- is abstract. We abstract it out of th4
c4 = runTrace $ runReader (loop =<< runC (th4 client)) (10::Int)
 where loop (Y x k) = trace (show (x::Int)) >> local (+(1::Int)) (k ()) >>= loop
       loop Done    = trace "Done"

       -- cl, client, ay are monomorphic bindings
       th4 cl = cl >> local (+(10::Int)) cl
       client = ay >> ay
       ay     = ask >>= \x -> yield (x::Int)

{-
10
11
21
21
Done
-}

-- Even more dynamic example
c5 = runTrace $ runReader (loop =<< runC (th client)) (10::Int)
 where loop (Y x k) = trace (show (x::Int)) >> local (\y->x+1) (k ()) >>= loop
       loop Done    = trace "Done"

       -- cl, client, ay are monomorphic bindings
       client = ay >> ay >> ay
       ay     = ask >>= \x -> yield (x::Int)

       -- There is no polymorphic recursion here
       th cl = do
         cl
         v <- ask
         (if v > (20::Int) then id else local (+(5::Int))) cl
         if v > (20::Int) then return () else local (+(10::Int)) (th cl)
{-
10
11
12
18
18
18
29
29
29
29
29
29
Done
-}

-- And even more
c7 = runTrace $
      runReader (runReader (loop =<< runC (th client)) (10::Int)) (1000::Double)
 where loop (Y x k) = trace (show (x::Int)) >>
                      local (\y->fromIntegral (x+1)::Double) (k ()) >>= loop
       loop Done    = trace "Done"

       -- cl, client, ay are monomorphic bindings
       client = ay >> ay >> ay
       ay     = ask >>= \x -> ask >>=
                 \y -> yield ((x::Int) + fromIntegral (round (y::Double)))

       -- There is no polymorphic recursion here
       th cl = do
         cl
         v <- ask
         (if v > (20::Int) then id else local (+(5::Int))) cl
         if v > (20::Int) then return () else local (+(10::Int)) (th cl)

{-
1010
1021
1032
1048
1064
1080
1101
1122
1143
1169
1195
1221
1252
1283
1314
1345
1376
1407
Done
-}

c7' = runTrace $
      runReader (runReader (loop =<< runC (th client)) (10::Int)) (1000::Double)
 where loop (Y x k) = trace (show (x::Int)) >>
                      local (\y->fromIntegral (x+1)::Double) (k ()) >>= loop
       loop Done    = trace "Done"

       -- cl, client, ay are monomorphic bindings
       client = ay >> ay >> ay
       ay     = ask >>= \x -> ask >>=
                 \y -> yield ((x::Int) + fromIntegral (round (y::Double)))

       -- There is no polymorphic recursion here
       th cl = do
         cl
         v <- ask
         (if v > (20::Int) then id else local (+(5::Double))) cl
         if v > (20::Int) then return () else local (+(10::Int)) (th cl)
{-
1010
1021
1032
1048
1048
1048
1069
1090
1111
1137
1137
1137
1168
1199
1230
1261
1292
1323
Done
-}

-- ------------------------------------------------------------------------
-- An example of non-trivial interaction of effects, handling of two
-- effects together
-- Non-determinism with control (cut)
-- For the explanation of cut, see Section 5 of Hinze ICFP 2000 paper.
-- Hinze suggests expressing cut in terms of cutfalse
--  ! = return () `mplus` cutfalse
-- where
--  cutfalse :: m a
-- satisfies the following laws
--   cutfalse >>= k  = cutfalse              (F1)
--   cutfalse | m    = cutfalse              (F2)
-- (note: m `mplus` cutfalse is different from cutfalse `mplus` m)
-- In other words, cutfalse is the left zero of both bind and mplus.
--
-- Hinze also introduces the operation call :: m a -> m a that
-- delimits the effect of cut: call m executes m. If the cut is 
-- invoked in m, it discards only the choices made since m was called.
-- Hinze postulates the axioms of call:
--
--   call false = false                          (C1)
--   call (return a | m) = return a | call m     (C2)
--   call (m | cutfalse) = call m                (C3)
--   call (lift m >>= k) = lift m >>= (call . k) (C4)
--
-- call m behaves like m except any cut inside m has only a local effect,
-- he says.

-- Hinze noted a problem with the `mechanical' derivation of backtracing
-- monad transformer with cut: no axiom specifying the interaction of 
-- call with bind; no way to simplify nested invocations of call.

-- We use exceptions for cutfalse
-- Therefore, the law ``cutfalse >>= k       = cutfalse''
-- is satisfied automatically since all exceptions have the above property.

data CutFalse = CutFalse deriving Typeable

cutfalse = throwError CutFalse

-- The interpreter -- it is like reify . reflect with a twist
-- Compare this implementation with the huge implementation of call
-- in Hinze 2000 (Figure 9)
-- Each clause corresponds to the axiom of call or cutfalse.
-- All axioms are covered.
-- The code clearly expresses the intuition that call watches the choice points
-- of its argument computation. When it encounteres a cutfalse request,
-- it discards the remaining choicepoints.

-- It completely handles CutFalse effects but not non-determinism
call :: Member Choose r => Eff (Exc CutFalse :> r) a -> Eff r a
call m = loop [] (admin m) where
 loop jq (Val x) = return x `mplus'` next jq          -- (C2)
 loop jq (E u) = case decomp u of
    Right (Exc CutFalse) -> mzero'  -- drop jq (F2)
    Left u -> check jq u

 check jq u | Just (Choose [] _) <- prj u  = next jq  -- (C1)
 check jq u | Just (Choose [x] k) <- prj u = loop jq (k x)  -- (C3), optim
 check jq u | Just (Choose lst k) <- prj u = next $ map k lst ++ jq -- (C3)
 check jq u = send (\k -> fmap k u) >>= loop jq      -- (C4)

 next []    = mzero'
 next (h:t) = loop t h

-- The signature is inferred
tcut1 :: (Member Choose r, Member (Exc CutFalse) r) => Eff r Int
tcut1 = (return (1::Int) `mplus'` return 2) `mplus'` 
         ((cutfalse `mplus'` return 4) `mplus'`
          return 5)

tcut1r = run . makeChoice $ call tcut1
-- [1,2]

tcut2 = return (1::Int) `mplus'` 
         call (return 2 `mplus'` (cutfalse `mplus'` return 3) `mplus'` 
               return 4) 
       `mplus'` return 5

-- Here we see nested call. It poses no problems...
tcut2r = run . makeChoice $ call tcut2
-- [1,2,5]

-- More nested calls
tcut3 = call tcut1 `mplus'` call (tcut2 `mplus'` cutfalse)
tcut3r = run . makeChoice $ call tcut3
-- [1,2,1,2,5]

tcut4 = call tcut1 `mplus'`  (tcut2 `mplus'` cutfalse)
tcut4r = run . makeChoice $ call tcut4
-- [1,2,1,2,5]


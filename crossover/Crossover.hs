{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE LambdaCase, DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ExistentialQuantification #-}

-- Generic cross-over: exchange suitable branches of two trees,
-- generically

module Crossover where

import Data.Data
import Data.Dynamic
import Control.Monad (liftM2)

-- Using extensible effects framework
-- I use Eff.hs in this directory
-- One can adjust the code to use any other ExtEff framework
import Eff
import OpenUnion1

-- First, we write an operation to traverse a data structure,
-- possibly replacing a node
-- The traversal stops when the tree is first updated

data UpdateFlag = New | Old deriving Typeable

traverse :: Member (State UpdateFlag) r =>
      (forall a. (Data a) => a -> Eff r a) ->
      (forall a. (Data a) => a -> Eff r a)
traverse f x = get >>= \case
  New -> return x
  Old -> do
    x <- f x
    get >>= \case
      Old -> traverse_children x
      New -> return x
 where
 traverse_children = gfoldl scan_kids return
 scan_kids builda x = liftM2 ($) builda (traverse f x)

-- Now, we differentiate traverse

-- When we resume with Nothing, we mean use the old value
data YieldD v = forall a. Data a => YieldD a (Maybe a -> v)
    deriving Typeable

instance Functor YieldD where
  fmap f (YieldD x k) = YieldD x (f . k)

yieldD :: (Data a, Member (State UpdateFlag) r, Member YieldD r) =>
          a -> Eff r a
yieldD x = send (inj . YieldD x) >>= \case
            Nothing -> return x
            Just x  -> put New >> return x

-- Status of a thread: done or reporting the value of the type a
data YD r a = YDone a | forall b. Data b => YD b (Maybe b -> Eff r (YD r a))

-- Launch a thread and report its status
-- runYD :: Typeable a => Eff (YieldD a :> r) w -> Eff r (YD r a)
runYD m = loop (admin m) where
 loop (Val x) = return (YDone x)
 loop (E u)   =
   handle_relay u loop $  \(YieldD x k) -> return $ YD x (loop . k)


-- Differentiated traversal
traverse_diff :: Data a => a -> Eff r (YD r (a,UpdateFlag))
traverse_diff x = runYD (runState (traverse yieldD x) Old)

-- First test: traverse a tree and print out all branches
printDyn x = trace . go $ toDyn x
 where
   go x | Just y <- fromDynamic x = show (y::Int)
   go x | Just y <- fromDynamic x = show (y::[Int])
   go x = show x

traverse_all :: (Member Trace r, Data a) => a -> Eff r a
traverse_all x = loop =<< traverse_diff x
 where
   loop (YDone (x,_)) = return x
   loop (YD x k) = printDyn x >> k Nothing >>= loop

tt1 = runTrace $ traverse_all [1::Int,2,3]
{-
[1,2,3]
1
[2,3]
2
[3]
3
[]
[1,2,3]
-}

zip_up :: YD r a -> Eff r a
zip_up (YDone x) = return x
zip_up (YD x k)  = zip_up =<< k (Just x)

-- Walk to a random branch in a data structure
random_walk :: (Member Choose r) => YD r a -> Eff r (YD r a)
random_walk y@YDone{} = return y
random_walk y@(YD x k) = return y `mplus'` (k Nothing >>= random_walk)

-- cross-over: randomly walk to a branch in tree x and tree y
-- and swap the branches if their types are compatible
-- After swapping, return the updated trees.
-- We permit only one swapping.
crossover :: (Member Choose r, Data a) => a -> a -> Eff r (a,a)
crossover x y = do
  tx <- traverse_diff x
  ty <- traverse_diff y
  tx <- random_walk tx  
  ty <- random_walk ty
  -- the swapping part
  case (tx,ty) of
    (YDone (x,_), YDone (y,_)) -> return (y,x)
    (YD x kx, YDone (y,_)) | Just x' <- cast x, Just y' <- cast y -> do
      (xnew,_) <- zip_up =<< kx (Just y')
      return (xnew,x')
    (YDone (y,_), YD x kx) | Just x' <- cast x, Just y' <- cast y -> do
      (xnew,_) <- zip_up =<< kx (Just y')
      return (x',xnew)
    (YD x kx, YD y ky) | Just x' <- cast x, Just y' <- cast y -> do
      (xnew,_) <- zip_up =<< kx (Just y')
      (ynew,_) <- zip_up =<< ky (Just x')
      return (xnew,ynew)
    _ -> mzero'



-- test data structures
tdata1 = [1::Int, 2, 3]
tdata2 = [10::Int, 20]
tdata3 = [[100::Int], [200, 300]]

-- Evaluiate the following to see the cross-over results

testc0 = run . makeChoice $ crossover (Just True) (Just False)

testc1 = run . makeChoice $ crossover tdata1 tdata2

testc2 = run . makeChoice $ crossover [tdata1] tdata3

data Tree = Leaf Int | Node Tree Tree
                       deriving (Show, Data, Typeable)

tree1 = Node (Leaf 1) (Leaf 2)
tree2 = Node (Leaf 10) (Node (Leaf 20) (Leaf 30))

testc3 = run . makeChoice $ crossover tree1 tree2



{-# LANGUAGE FlexibleContexts, DeriveDataTypeable #-}

-- Sharing of top-level bindings
-- Solving Takayuki Muranushi's problem
-- http://www.haskell.org/pipermail/haskell-cafe/2012-July/102287.html

module TopSharing where

import qualified Data.Map as M
import Control.Monad.State
import Control.Applicative

earthMass :: NonDet Double
earthMass = memoSta "earthMass" $
            msum $ map return [1,10,100]

sunMass :: NonDet Double
sunMass = memoSta "sunMass" mass
 where mass = (*) <$> proportion <*> earthMass
       proportion = msum $ map return [9,10,11]

marsMass :: NonDet Double
marsMass = memoSta "marsMass" mass
 where mass = (*) <$> proportion <*> earthMass
       proportion = msum $ map return [0.09, 0.1, 0.11]

sunPerMars :: NonDet Double
sunPerMars = (/) <$> sunMass <*> marsMass

sunPerMars_run = runShare sunPerMars
sunPerMars_range = (minimum sunPerMars_run, maximum sunPerMars_run)

stateAfter = runShare $ sunPerMars >> get


type UniqObj = String

type NonDet a = StateT FirstClassStore [] a
data Key = KeySta UniqObj
         deriving (Show, Ord, Eq)

data FirstClassStore =
    FirstClassStore { freshKey :: Int, store :: M.Map Key String } deriving Show

emptyStore :: FirstClassStore
emptyStore = FirstClassStore { freshKey = 1, store = M.empty }


insertVal :: (Show a, MonadState FirstClassStore m) => Key -> a -> m ()
insertVal key val =
  modify (\s -> s { store = M.insert key (show val) (store s) })

lookupVal :: (Show a, Read a, MonadState FirstClassStore m) => Key -> m (Maybe a)
lookupVal key =
   liftM (liftM read . M.lookup key) (gets store)
 where err = error $ "lookupVal: bad key " ++ show key


memoSta :: (Show a, Read a, MonadState FirstClassStore m) => UniqObj -> m a -> m a
memoSta trep a = memoKey (KeySta trep) a

memoKey :: (Show a, Read a, MonadState FirstClassStore m) => Key -> m a -> m a
memoKey key a = do
  valM <- lookupVal key
  case valM of
    Just x  -> return x
    Nothing -> do
               x <- a
               insertVal key $! x
               return x

runShare :: Monad m => StateT FirstClassStore m a -> m a
runShare m = evalStateT m emptyStore



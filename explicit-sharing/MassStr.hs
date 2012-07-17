
{-# LANGUAGE FlexibleContexts, DeriveDataTypeable #-}

-- Sharing of top-level bindings
-- Solving Takayuki Muranushi's problem
-- http://www.haskell.org/pipermail/haskell-cafe/2012-July/102287.html

module TopSharing where

import qualified Data.Map as M
import Control.Monad.State
import Data.Dynamic
import Control.Applicative

data EarthMass deriving Typeable
earthMass :: NonDet Double
earthMass = memoSta (typeOf (undefined::EarthMass)) $
            msum $ map return [1,10,100]


-- Let's pretend this is another separate module
-- It imports earthMass and exports sunMass
-- Muranushi: ``Let's also pretend that we can measure the other
-- bodies' masses only by their ratio to the Earth mass, and
-- the measurements have large uncertainties.''

data SunMass deriving Typeable
sunMass :: NonDet Double
sunMass = memoSta (typeOf (undefined::SunMass)) mass
 where mass = (*) <$> proportion <*> earthMass
       proportion = msum $ map return [12,24,36]

-- Let's pretend this is yet another separate module
-- It imports earthMass and exports marsMass

data MarsMass deriving Typeable
marsMass :: NonDet Double
marsMass = memoSta (typeOf (undefined::MarsMass)) mass
 where mass = (*) <$> proportion <*> earthMass
       proportion = msum $ map return [2,3,4]

-- This is the main module, importing the masses of the three bodies
-- It computes ``how many Mars mass object can we create
-- by taking the sun apart?''
-- This code is exactly the same as in Takayuki Muranushi's message
-- His question: ``Is there a way to represent this?
-- For example, can we define earthMass'' , sunMass'' , marsMass'' all
-- in separate modules, and yet have (length $ sunPerMars'' == 27) ?

sunPerMars :: NonDet Double
sunPerMars = (/) <$> sunMass <*> marsMass

sunPerMars_run = runShare sunPerMars
sunPerMars_run_len = length sunPerMars_run
-- 27


-- The following is essentially Control.Monad.Sharing.Memoization
-- with one important addition
-- Can you spot the important addition?

type NonDet a = StateT FirstClassStore [] a
data Key = KeySta TypeRep
         deriving (Show, Ord, Eq)

data FirstClassStore =
    FirstClassStore { freshKey :: Int, store :: M.Map Key Dynamic }

emptyStore :: FirstClassStore
emptyStore = FirstClassStore { freshKey = 1, store = M.empty }


insertVal :: (Typeable a, MonadState FirstClassStore m) => Key -> a -> m ()
insertVal key val =
  modify (\s -> s { store = M.insert key (toDyn val) (store s) })

lookupVal :: (Typeable a, MonadState FirstClassStore m) => Key -> m (Maybe a)
lookupVal key =
   liftM (liftM (flip fromDyn err) . M.lookup key) (gets store)
 where err = error $ "lookupVal: bad key " ++ show key


memoSta :: (Typeable a, MonadState FirstClassStore m) => TypeRep -> m a -> m a
memoSta trep a = memoKey (KeySta trep) a

memoKey :: (Typeable a, MonadState FirstClassStore m) => Key -> m a -> m a
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


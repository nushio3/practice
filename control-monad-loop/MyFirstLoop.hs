{-# OPTIONS -Wall #-}

module MyFirstLoop where
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Loop

-- import Control.Monad.Trans.Class    (MonadTrans(lift))
-- instance MonadTrans (LoopT c e) where
--     lift m = LoopT $ \_ _ cont -> m >>= cont
-- instance MonadIO m => MonadIO (LoopT c e m) where
--     liftIO = lift . liftIO


-- *introduction 
{- $
 Hello, this is a program written to study the 
 <http://hackage.haskell.org/package/control-monad-loop> package.
-}

-- | This list contains all integers
-- starting from zero. lets check for it.
-- 
-- >>> print $ take 5 $ xs
-- [0,1,2,3,4]
--
xs :: [Int]
xs = [0..]


-- | This is main.
-- let us see how it works.
--
-- >>> main
-- 0123456789
--
main :: IO ()
main = do
  foreach xs $ \i-> do
    liftIO $ putStr $ show i
    when (i >= 9) exit



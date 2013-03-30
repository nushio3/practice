{-# LANGUAGE FlexibleContexts #-}

module Main where

import Control.Monad.Morph
import Control.Monad.State
import Control.Monad.Writer
import Control.Monad.Trans.Class
import Data.Functor.Identity


generalize :: (Monad m) => Identity a -> m a
generalize m = return (runIdentity m)

tick :: State Int ()
tick = modify (+1)

tick' :: MonadState Int m => m ()
tick' = modify (+1)

tock :: StateT Int IO ()
tock = do
  hoist generalize tick
  liftIO $ putStrLn "tock!"

tock' :: (MonadState Int m, MonadIO m) => m ()
tock' = do
  tick'
  liftIO $ putStrLn "tock'!"


save :: StateT Int (Writer String) ()
save = do
  n <- get
  tell (show [n])

save' :: (MonadState Int m, MonadWriter String m) => m ()
save' = do
  n <- get
  tell (show [n])


prog :: StateT Int (WriterT String IO) ()
prog = replicateM_ 4 $ do
  hoist lift tock
  hoist (hoist generalize) save

prog' :: (MonadState Int m, MonadWriter String m, MonadIO m) => m ()
prog' = replicateM_ 4 $ do
  tock'
  save'

main :: IO ()
main = do
  x <- flip execStateT 40 $ do
    tock
    tock
  print x

  str <- execWriterT $ (runStateT prog 38)
  putStrLn str

  str <- execWriterT $ (runStateT prog' 38)
  putStrLn str

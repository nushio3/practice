{-# LANGUAGE ConstraintKinds, FlexibleContexts #-}
import Control.Monad.State

type M = StateT Int IO

type HasInt s = Num s

seven :: (MonadState s m, HasInt s) => m ()
seven = do
  x <- get
  put $ 7 * x

prog :: M ()
prog = do
  seven
  y <- get
  liftIO $ print y


main :: IO ()
main = do
  evalStateT prog 6

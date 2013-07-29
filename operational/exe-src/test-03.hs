{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}

module Main where

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Operational.Mini
import Control.Monad.Operational.Class
import Control.Monad.Trans.Operational.Mini

data PLang a where
    ReadLog :: PLang Log
    PlayHand  :: Hand -> PLang ()

type Log = [Hand]
data Hand = G | C | P deriving (Eq, Ord, Read, Show)

versus :: Hand -> Hand -> String
versus G C = "you win"
versus C P = "you win"
versus P G = "you win"
versus x y
  | x == y    = "draw"
  | otherwise = "you lose"


playHand :: Operational PLang m => Hand -> m ()
playHand = singleton . PlayHand

readLog :: Operational PLang m => m Log
readLog = singleton ReadLog

runGame :: Program PLang () -> IO ()
runGame player1 = go [] player1
  where
    go :: Log -> Program PLang () -> IO ()
    go gameLog p1 = do
      let vp :: ReifiedProgram PLang ()
          vp = cloneProgram p1
      case vp of
        Return _           -> return ()
        ReadLog :>>= kp    -> go gameLog (fromReified $ kp $ reverse gameLog)
        PlayHand h :>>= kp -> go (h : gameLog) (fromReified $ kp ())


cloneProgramT :: Operational t m => ProgramT t m a -> m a
cloneProgramT (ProgramT m) = m return (\t c -> singleton t >>= c)

main :: IO ()
main = runGame aiPlayer

humanPlayer :: (Operational PLang m, MonadIO m) => m ()
humanPlayer = forever $ do
  str <- liftIO getLine
  case str of
    ('g':_) -> playHand G
    ('c':_) -> playHand C
    ('p':_) -> playHand P
    ('?':_) -> do
      gameLog <- readLog
      liftIO $ print gameLog


-- aiPlayer :: (Operational PLang m, Monad m) => m ()
aiPlayer :: Program PLang ()
aiPlayer = forever $ playHand G
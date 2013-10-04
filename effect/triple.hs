{-# LANGUAGE TemplateHaskell #-}

import Control.Monad.State
import Control.Lens
import Control.Monad.IO.Class

data Aegis = Aegis { _alpha :: Int }
  deriving (Eq,Show)
           
makeClassy ''Aegis

data Beast = Beast { _beta :: Double }
  deriving (Eq,Show)
           
makeClassy ''Beast

data Chopin = Chopin { _gamma :: String }
  deriving (Eq,Show)

makeClassy ''Chopin

data Suumo = Suumo { _suumoAegis :: Aegis, _suumoBeast :: Beast, _suumoChopin :: Chopin }
  deriving (Eq,Show)
           
makeClassy ''Suumo

instance HasAegis Suumo where aegis = suumoAegis
instance HasBeast Suumo where beast = suumoBeast
instance HasChopin Suumo where chopin = suumoChopin

suumo0 :: Suumo
suumo0 = Suumo (Aegis 42) (Beast 6) (Chopin "Nocturne")

addAegis :: (HasAegis s, MonadState s m) => Int -> m () 
addAegis x = aegis . alpha += x

replicateBeast :: (HasBeast s, MonadState s m) => Double -> m ()
replicateBeast x = beast . beta *= x

bestOfChopin :: (HasChopin s, MonadState s m, MonadIO m) => m String
bestOfChopin = do
  oldStr <- use $ chopin . gamma
  liftIO $ putStrLn $ "What is your most favorite chopin?"
  newStr <- liftIO $ getLine
  chopin . gamma .= newStr
  return $ oldStr ++ " was kicked."
  

prog :: (HasAegis s, HasBeast s, HasChopin s, MonadIO m, MonadState s m) => m ()
prog = do
  replicateBeast 1.11
  addAegis 30
  msg <- bestOfChopin
  liftIO $ putStrLn msg
  

main :: IO ()
main = do
  suumo1 <- execStateT prog suumo0
  print suumo1
  
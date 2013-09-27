{-# LANGUAGE TemplateHaskell #-}

import Control.Monad.State
import Control.Lens
import Control.Monad.IO.Class

data Attribute = Attribute { _writable :: Bool, _readable :: Bool }
  deriving (Eq,Show)
           
makeClassy ''Attribute
{--- the above TH creates ----

class HasAttribute t where
  attribute :: Lens' t Attribute
  readable :: Lens' t Bool
  writable :: Lens' t Bool

instance HasAttribute Attribute where
  attribute = id
-}

data Battery = Battery { _level :: Double }
  deriving (Eq,Show)
           
makeClassy ''Battery

data Chopin = Chopin { _title :: String }
  deriving (Eq,Show)

makeClassy ''Chopin

data Robot 
  = Robot 
  { _robotAttribute :: Attribute
  , _primaryBattery :: Battery
  , _secondaryBattery :: Battery
  , _robotChopin :: Chopin 
  }
  deriving (Eq,Show)
           
makeClassy ''Robot

instance HasAttribute Robot where attribute = robotAttribute
instance HasBattery Robot where battery = primaryBattery
instance HasChopin Robot where chopin = robotChopin

robot0 :: Robot
robot0 = Robot (Attribute False False) (Battery 0) (Battery 100) 
         (Chopin "Nocturne")

enableWrite :: (HasAttribute s, MonadState s m) => m () 
enableWrite = writable ||= True

addBattery :: (HasBattery s, MonadState s m) => Double -> m ()
addBattery x = level += x

moveBattery :: (HasBattery s, HasRobot s, MonadState s m) => Double -> m ()
moveBattery x = do
  primaryBattery . level -= x
  secondaryBattery . level -= x
  level += 2*x


bestOfChopin :: (HasAttribute s, HasChopin s, MonadState s m, MonadIO m) => m ()
bestOfChopin = do
  wf <- use $ attribute . writable
  when wf $ do
    oldStr <- use $ chopin . title
    liftIO $ putStrLn $ "What is your most favorite chopin?"
    newStr <- liftIO $ getLine
    chopin . title .= newStr

  

prog :: (HasAttribute s, HasBattery s, HasChopin s, MonadIO m, MonadState s m) => m ()
prog = do
  addBattery 44
  moveBattery 28
  enableWrite
  bestOfChopin
  

main :: IO ()
main = do
  robot1 <- execStateT prog robot0
  print robot1
  
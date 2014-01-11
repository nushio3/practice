{-# LANGUAGE TemplateHaskell #-}
import Control.Lens

data Object = Object
  { _position :: Double
  , _velocity :: Double 
  , _mass     :: Double }

makeLenses ''Object

kineticEnergy :: Getter Object Double
kineticEnergy = to $ go where
  go obj = 0.5 * (obj ^. mass) * (obj ^. velocity)^2

potentialEnergy :: Getter Object Double
potentialEnergy = to $ go where
  go obj = 0.42 * (obj ^. position)^2

lagrangian :: Getter Object Double
lagrangian = to $ go where
  go obj = (obj ^. kineticEnergy) - (obj ^. potentialEnergy)


ball = Object 2 3 5

main = do
  print $ ball ^. kineticEnergy
  print $ ball ^. lagrangian

{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}
import Control.Lens

data Object = Object
  { _position :: Double
  , _velocity :: Double 
  , _mass     :: Double }

makeLenses ''Object

-- The Type in my mind
-- coto :: ((Getter s b -> b) -> a) -> Getter s a

-- The Type obtained by ghci
coto :: (Functor f, Conjoined p, Contravariant f) =>
     ((Getting a s a -> a) -> a1) -> p a1 (f a1) -> p s (f s)
coto go = to (go . (^.))

kineticEnergy :: Getter Object Double
kineticEnergy = coto go where
  go obj = 0.5 * obj mass * obj velocity ^2

potentialEnergy :: Getter Object Double
potentialEnergy = coto go where
  go obj = 0.42 * obj position ^2

lagrangian :: Getter Object Double
lagrangian = coto go where
  go obj = obj kineticEnergy - obj potentialEnergy


ball = Object 2 3 5

main = do
  print $ ball ^. kineticEnergy
  print $ ball ^. lagrangian

{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ViewPatterns #-}
import Control.Lens

data Object = Object
  { _position :: Double
  , _velocity :: Double 
  , _mass     :: Double 
  , _name     :: String }

makeLenses ''Object

ball = Object 2 3 5 "SoccerBall"

main = do
  print $ ball ^. kineticEnergy
  print $ kineticEnergyFunc ball

  print $ ball ^. potentialEnergy
  print $ potentialEnergyFunc ball

  print $ ball ^. lagrangian
  print $ lagrangianFunc ball

  putStrLn $ messageFunc ball

  print $ shini 1

kineticEnergy :: Getter Object Double
kineticEnergy = to (go . (^.)) where
  go obj = 0.5 * obj mass * obj velocity ^2

kineticEnergyFunc :: Object -> Double
kineticEnergyFunc = go . (^.) where
  go obj = 0.5 * obj mass * obj velocity ^2


potentialEnergy :: Getter Object Double
potentialEnergy = to (go . (^.)) where
  go obj = 0.42 * obj position ^2

potentialEnergyFunc :: Object -> Double
potentialEnergyFunc = go . (^.) where
  go obj = 0.42 * obj position ^2


lagrangian :: Getter Object Double
lagrangian = to (go . (^.)) where
  go obj = obj kineticEnergy - obj potentialEnergy

lagrangianFunc :: Object -> Double
lagrangianFunc = go . (^.) where
  go obj = obj kineticEnergy - obj potentialEnergy
  



message :: Getter Object String
message = to go where
  go obj = 
    unwords
      [ "Lagragian of"
      , obj's name 
      , "is" 
      , show $ obj's lagrangian ]
    where
      obj's = (obj ^.)

messageFunc :: Object -> String
messageFunc obj = 
  unwords
    [ "Lagragian of"
    , obj's name 
    , "is" 
    , show $ obj's lagrangian ]
  where
    obj's = (obj ^.)


{-- -- this doesn't typecheck
message2 :: Getter Object String
message2 = to (go . (^.)) where
  go obj = unwords
    [ "Lagragian of"
    , obj name 
    , "is" 
    , show $ obj lagrangian ]
--}


{-- -- ViewPatterns test. 
-- You can take this
shini :: Int -> Int
shini x = let y = x*6 in 3*y+4*y
--}

-- and rewrite it as follows
shini :: Int -> Int
shini ((*6) -> y) = 3*y+4*y


-- Now this works well
message2 :: Getter Object String
message2 = to go where
  go obj = let obj's = (obj ^.) in unwords
    [ "Lagragian of"
    , obj's name 
    , "is" 
    , show $ obj's lagrangian ]

{--
-- But this can not go into viewpatterns.
message4 :: Getter Object String
message4 = to go where
  go ((. (^.)) -> obj's) = unwords
    [ "Lagragian of"
    , obj's name 
    , "is" 
    , show $ obj's lagrangian ]

--}


{-
messageFunc :: Object -> String
messageFunc = goM . (^.) 

--goM :: Functor f =>
--     (((a -> f a) -> Object -> f Object) -> a) -> String

goM :: (forall b. Getting b Object b -> b) -> String
goM obj = unwords
    [ "Lagragian of"
    , obj name 
    , "is" 
    , show $ obj lagrangian ]
  
-}

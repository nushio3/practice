{-# LANGUAGE ConstraintKinds, DataKinds, TypeFamilies,TypeOperators #-}
import Data.Metrology
import Data.Metrology.Internal
import Data.Metrology.SI
import Data.Metrology.Show

earthMass, solarMass :: Mass
solarMass = 1.99e33 % Gram
earthMass = 5.97e24 % kilo Gram

ratio ::  (Normalize (Reorder d1 d2 @@- d2) ~ '[], Fractional n) 
           => Qu d1 l n -> Qu d2 l n -> n
ratio x y = (x |/| y) # Number

main :: IO ()
main = do
  print solarMass
  print earthMass  
  putStrLn $ "mass ratio of the Earth and the Sun is: " ++ show (ratio earthMass solarMass)
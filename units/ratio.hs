{-# LANGUAGE ConstraintKinds, DataKinds, TypeFamilies,TypeOperators #-}
import Data.Metrology
import Data.Metrology.Internal
import Data.Metrology.SI
import Data.Metrology.Show

earthMass, solarMass :: Mass
solarMass = 1.99e33 % Gram
earthMass = 5.97e24 % kilo Gram

nrg1 :: Energy
nrg1 = 20 % Joule

nrg2 = 300 % ( Meter :* Gram :* Meter :/ Second  :/ Second )
nrg3 = 60 % ( Meter  :/ Second:* Meter :/ Second :* Gram )
         




ratio ::  (Normalize (Reorder d1 d2 @@- d2) ~ '[], Fractional n) 
           => Qu d1 l n -> Qu d2 l n -> n
ratio x y = (x |/| y) # Number

main :: IO ()
main = do
  print solarMass
  print earthMass  
  putStrLn $ "mass ratio of the Earth and the Sun is: " ++ show (ratio earthMass solarMass)
  
  
  print $ nrg1
  print $ nrg2  
  print $ nrg1 |+| nrg2 |+| nrg3
  print $ nrg2 `ratio` nrg3


{-

nrg2 and nrg3 have been given dimensions with different order of base
dimensions. However, their dimensions are both equivalent to Energy.
Therefore, the ratio between nrg2 and nrg3 are nondimensional. 

> :t nrg2
nrg2
  :: Qu
       '['F Data.Metrology.SI.Dims.Time ('P ('P 'Zero)),
         'F Data.Metrology.SI.Dims.Length ('S ('S 'Zero)),
         'F Data.Metrology.SI.Dims.Mass One]
       'DefaultLCSU
       Double
> :t nrg3
nrg3
  :: Qu
       '['F Data.Metrology.SI.Dims.Mass One,
         'F Data.Metrology.SI.Dims.Time ('P ('P 'Zero)),
         'F Data.Metrology.SI.Dims.Length ('S ('S 'Zero))]
       'DefaultLCSU
       Double
> ratio nrg2 nrg3
5.0


-}
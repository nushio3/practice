{- Based on "Do we Need Dependent Types?", BRICS RS-01-10, 
   Daniel Fridlender and Mia Indrika (2001).

 Special thanks to Jason Dagit and haskell-cafe.  -}

import qualified Data.Vector as V
import           Data.Key


xs1, xs2, xs3, xs4 :: V.Vector Int
xs1 = V.fromList [100..109]
xs2 = V.fromList [200..209]
xs3 = V.fromList [300..309]
xs4 = V.fromList [400..409]

main = do
  print $ fmap (*2) xs1
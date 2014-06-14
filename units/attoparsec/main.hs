{-# LANGUAGE DataKinds, TypeOperators, GeneralizedNewtypeDeriving #-}

import qualified Data.Metrology.SI.Dims as D
import Data.Metrology.SI(Atto(..), Second(..))
import Data.Metrology
import Data.Metrology.Show
import Data.Ratio hiding ((%))

newtype FracInt = MkFI Integer
  deriving (Num, Integral, Real, Enum, Ord, Eq)

instance Show FracInt where
  show (MkFI x) = show x

instance Fractional FracInt where
  fromRational r = MkFI (numerator r `div` denominator r)
  (/) = div

-- assuming that you're only interested in storing times, not other dimensions
type AttoSecLCSU = MkLCSU '[(D.Time, Atto :@ Second)]

type Time = MkQu_DLN D.Time AttoSecLCSU FracInt
mkAS :: Integer -> Time
mkAS x = MkFI x % Atto :@ Second

main = do
  putStrLn "hello"
  print $ (mkAS 1234) |+| (mkAS 4567)
  print $ (mkAS 1) |/|  (MkFI 2 % Number)  

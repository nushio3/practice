{- Based on "Do we Need Dependent Types?", BRICS RS-01-10, 
   Daniel Fridlender and Mia Indrika (2001).

 Special thanks to Jason Dagit and haskell-cafe.  -}

import qualified Data.Vector as V
import           Data.Key
import           Prelude hiding (zipWith, succ)
import           Text.Printf


instance Zip V.Vector where
  zipWith = V.zipWith         

xs1, xs2, xs3, xs4 :: V.Vector Int
xs1 = V.fromList [100..102]
xs2 = V.fromList [200..202]
xs3 = V.fromList [300..302]
xs4 = V.fromList [400..402]

fary2 :: Int -> Int -> String
fary2 = printf "%d<%d"

fary4 :: Int -> Int ->  Int ->  Int -> Int
fary4 a b c d = b*c - a*d

(<<) :: Zip f => f (a->b) -> f a -> f b
fs << as = zipWith ($) fs as

succ :: (Zip f) => (f b -> c) -> (f (a->b)) -> f a -> c
succ n fs as = n (fs << as)

zipWN n f xs = n (fmap f xs)

zipWith4 f as1 as2 as3 as4 = (fmap f as1) << as2 << as3 << as4

zipWith4' = zipWN three
  where three = succ (succ (succ (id)))

main = do
  print $ fmap (*2) xs1
  print $ zipWith fary2 xs1 xs2
  print $ zipWith4 fary4 xs1 xs2 xs3 xs4
  print $ zipWith4' fary4 xs1 xs2 xs3 xs4
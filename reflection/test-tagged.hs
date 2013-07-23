{- calculate orbital periods of some planets (in MKSA units) -}

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Data.Reflection
import Text.Printf


-- | tagged reflection

set :: forall tag a r. tag -> a -> (Given (tag, a) => r) -> r
set tag0 val = give (tag0, val)

use :: forall tag a. Given (tag, a) => tag -> a
use _ = snd (given :: (tag, a))


-- | definition of concepts

data Radius = Radius
data CentralMass = CentralMass


-- | you can calculate the orbital period of a planet given its
--   orbitar radius and the mass of the central star.

period :: (Given (Radius, Double), Given (CentralMass, Double) ) => Double
period = 2 * pi / sqrt((use CentralMass) * 6.67384e-11 / (use Radius)**3)

-- | some utility values

year :: Double
year = 365*24*60*60

day :: Double
day = 24*60*60



main :: IO ()
main = do
  give (Radius, 1.50e11) $ give (CentralMass, 1.99e30) $
    printf "The orbital period of the Earth is %f years\n" $ period/year

  set Radius 7.78e11 $ set CentralMass 1.99e30 $
    printf "The orbital period of Jupiter is %f years\n" $ period/year

  set Radius 7.79e9  $ set CentralMass 2.11e30 $
    printf "The orbital period of 51 Peg b is %f days\n" $ period/day

{-

$ runhaskell test-tagged.hs
The orbital period of the Earth is 1.0043745892820515 years
The orbital period of Jupiter is 11.863920379400026 years
The orbital period of 51 Peg b is 4.213501854937773 days

-}

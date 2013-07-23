{- calculate orbital periods of some planets (in MKSA units) -}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
module Main where

import Data.Reflection
import Data.Tagged
import Text.Printf


-- | tagged reflection

infix 1 `is`

is :: forall tag a r. tag -> a -> (Given (Tagged tag a) => r) -> r
is _ val = give (Tagged val)

the :: forall tag a. Given (Tagged tag a) => tag -> a
the _ = unTagged (given :: Tagged tag a)


-- | definition of the concepts

data Radius = Radius
data CentralMass = CentralMass

type tag ::: a = Given (Tagged tag a)

-- | you can calculate the orbital period of a planet given its
--   orbital radius and the mass of the central star.

period :: (Radius ::: Double, CentralMass ::: Double) => Double

period = 2 * pi / sqrt((the CentralMass) * 6.67384e-11 / (the Radius)**3)

-- | some utility values

year :: Double
year = 365*24*60*60

day :: Double
day = 24*60*60



main :: IO ()
main = do
  Radius `is` 1.50e11 $ CentralMass `is` 1.99e30 $
    printf "The orbital period of the Earth is %3.2f years\n" $ period/year

  Radius `is` 7.78e11 $ CentralMass `is` 1.99e30 $
    printf "The orbital period of Jupiter is %3.2f years\n" $ period/year

  Radius `is` 7.79e9  $ CentralMass `is` 2.11e30 $
    printf "The orbital period of 51 Peg b is %3.2f days\n" $ period/day

{-

$ runhaskell test-tagged.hs
The orbital period of the Earth is 1.00 years
The orbital period of Jupiter is 11.86 years
The orbital period of 51 Peg b is 4.21 days

-}

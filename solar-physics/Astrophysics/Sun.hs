{-# LANGUAGE TypeOperators, QuasiQuotes #-}

module Astrophysics.Sun where

import Data.Metrology
import Data.Metrology.SI
import Data.Metrology.SI.Parser
import qualified Data.Metrology.SI.Dims as D


massOfSun :: Mass
massOfSun = 1.989e30 % [si| kg |]

solarLuminousity :: MkQu_D (D.Energy :/ D.Time)
solarLuminousity = redim $ 3.839e30 % [si| W |]

hydrogenMassFractionOfSun :: Count
hydrogenMassFractionOfSun = 0.75 % [si| |]

heliumMassFractionOfSun :: Count
heliumMassFractionOfSun = 0.25 % [si| |]
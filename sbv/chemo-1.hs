import Control.Monad
import Data.SBV

main = do
   (print =<<) $ allSat $ do
     nH    <- sReal "nH"
     nC    <- sReal "nC"
     nH2O  <- sReal "nH2O"
     nO    <- sReal "nO"
     nCH4  <- sReal "nCH4"
     let
       chems = [nH, nC, nH2O, nO, nCH4]
       evapH = 1e3 * catalyst
       absorbH = 3 * nH
       evapO = 1e2 * catalyst
       absorbO = 3 * nO
       evapC = 0.5e2 * catalyst
       absorbC = 3 * nC
       catalyst =  (1 + 1e-12 * nC^6)
       formH2O = 2.3 * nH * nO
       dissocH2O = 42 * nH2O
       formCH4 = 1.3 * nC * nO * catalyst
       dissocCH4 = 56 * nCH4
     sequence [constrain $ n .>= 0| n <- chems]

     solve [ evapH - absorbH
              - 2*formH2O + 2*dissocH2O
              - 4*formCH4 + 4*dissocCH4
              .==0
           , evapC - absorbC
              - formCH4 + dissocCH4
              .==0
           , evapO - absorbO
              - formH2O + dissocH2O
              .==0
           ,       formH2O -   dissocH2O  .==0
           ,       formCH4 -   dissocCH4  .==0
           ]
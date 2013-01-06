import BinarySearch

import Data.SBV
import Text.Printf

main = do
  cost <- budgetSatProb
  putStrLn $ "budget = " ++ show(fromRational cost :: Double)

budgetSatProb :: IO Rational
budgetSatProb = binarySearchWith z3 defaultBSOpts p
  where
    p :: Rational -> Predicate
    p budgetReal = do
      let budget :: SReal
          budget = fromRational budgetReal

      numNodes <- sReal "Nodes"
      constrain $ numNodes .>= 1
      tflopsPerPU <- sReal "TFlops/PU"

      puKind <- sWord8 "PUKind"
      constrain $ puKind .<= 2
      constrain $ 
        (puKind .== 0 &&&  1 .<= tflopsPerPU &&& tflopsPerPU .<=  3) |||
        (puKind .== 1 &&&  6 .<= tflopsPerPU &&& tflopsPerPU .<= 12) |||
        (puKind .== 2 &&& 10 .<= tflopsPerPU &&& tflopsPerPU .<= 20)

      let totalCost  = costPerNode * numNodes
          totalFlops = tflopsPerPU * 10^12 * numNodes
          
          costPerNode = costPerPU
          costPerPU =
            ite (puKind .== 0) (tflopsPerPU * 1e4 + 7700) $
            ite (puKind .== 1) (3.99e4 * tflopsPerPU) $
                               (8e4 * tflopsPerPU)
      constrain $ totalFlops .>= 10^18
      constrain $ budget .>= totalCost

      return (true :: SBool)

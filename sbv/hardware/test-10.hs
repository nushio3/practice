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

      constrain $ tflopsPerPU .>=  1  
      constrain $ tflopsPerPU .<= 20 

      let totalCost  = costPerNode * numNodes
          totalFlops = tflopsPerPU * 10^12 * numNodes
          
          costPerNode = costPerPU
          costPerPU =
            ite (tflopsPerPU .<= 3) (tflopsPerPU * 1e4 + 7700) $
            ite (tflopsPerPU .<= 9) (tflopsPerPU * 4e4) $
                                    (tflopsPerPU * 8e4)
      constrain $ totalFlops .>= 10^18
      constrain $ budget .>= totalCost

      return (true :: SBool)

import BinarySearch

import Data.SBV
import Text.Printf

main = do
  cost <- budgetSatProb
  putStrLn $ "budget = " ++ show cost

budgetSatProb :: IO Int64
budgetSatProb = binarySearchWith z3 defaultBSOpts p
  where
    p :: Int64 -> Predicate
    p budgetInt64 = do
      let budget :: SInt64
          budget = fromIntegral budgetInt64

      numNodes <- sInt64 "Nodes"
      constrain $ numNodes .>= 1
      cpuAPerNode <- sInt64 "CPU-A/Node"
      cpuBPerNode <- sInt64 "CPU-B/Node"
      cpuCPerNode <- sInt64 "CPU-C/Node"
      constrain $ cpuAPerNode .>= 0
      constrain $ cpuBPerNode .>= 0
      constrain $ cpuCPerNode .>= 0
      constrain $ cpuAPerNode + cpuBPerNode + cpuCPerNode .== 1

      let totalCost  = costPerNode  * numNodes
          totalFlops = flopsPerNode * numNodes

          costPerNode =
            cpuAPerNode *  27770 +
            cpuBPerNode * 399000 +
            cpuCPerNode * 892500

          flopsPerNode =
            cpuAPerNode *  2 * 10^12 +
            cpuBPerNode *  9 * 10^12 +
            cpuCPerNode * 15 * 10^12


          numCPUs = cpuAPerNode * numNodes

      constrain $ totalFlops .>= 10^18
      constrain $ budget .>= totalCost

      return (true :: SBool)

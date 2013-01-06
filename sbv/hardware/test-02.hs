import BinarySearch

import Data.SBV
import Text.Printf

main = do
  cost <- budgetSatProb
  printf "budget = %d\n" cost

budgetSatProb :: IO Integer
budgetSatProb = binarySearchWith z3 defaultBSOpts p
  where
    p :: Integer -> Predicate
    p budgetInteger = do
      let budget :: SInteger
          budget = fromIntegral budgetInteger

      numNodes <- sInteger "Nodes"
      constrain $ numNodes .>= 1
      cpuAPerNode <- sInteger "CPU-A/Node"
      cpuBPerNode <- sInteger "CPU-B/Node"
      cpuCPerNode <- sInteger "CPU-C/Node"
      constrain $ cpuAPerNode .>= 0
      constrain $ cpuBPerNode .>= 0
      constrain $ cpuCPerNode .>= 0
      constrain $ cpuAPerNode + cpuBPerNode + cpuCPerNode .> 0

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

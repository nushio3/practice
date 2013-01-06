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

      numNodes <- sInteger "Nodes"
      constrain $ numNodes .>= 1
      cpuAPerNode <- sInteger "CPU-A/Node"
      cpuBPerNode <- sInteger "CPU-B/Node"
      cpuCPerNode <- sInteger "CPU-C/Node"

      constrain $ cpuAPerNode .>= 0
      constrain $ cpuBPerNode .>= 0
      constrain $ cpuCPerNode .>= 0
      constrain $ cpuAPerNode + cpuBPerNode + cpuCPerNode .<= 4

      let totalCost  = costPerNode  * numNodes
          totalFlops = flopsPerNode * numNodes

          costPerNode = toSReal $
            cpuAPerNode *  27770 +
            cpuBPerNode * 399000 +
            cpuCPerNode * 892500

          flopsPerNode = 
            cpuAPerNode *  2 * 10^12 +
            cpuBPerNode *  9 * 10^12 +
            cpuCPerNode * 15 * 10^12



      constrain $ totalFlops .>= 10^18
      constrain $ budget .>= totalCost

      return (true :: SBool)

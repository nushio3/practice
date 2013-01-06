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

      gbytesPerMem <- sReal "Mem(GB)"
      constrain $ gbytesPerMem .>=   0.1  
      constrain $ gbytesPerMem .<= 512 

      auxPowerGW <- sReal "auxPowerSupply(GW)"
      constrain $ auxPowerGW .>= 0

      let totalCost  = costPerNode * numNodes + runningCostPerYear * runningYear
          totalWatt  = wattPerNode * numNodes 

          totalFlops = tflopsPerPU * 10^12 * numNodes
          totalMem   = gbytesPerMem* 10^9 * numNodes
          totalMemBW = gbpsPerMem  * 10^9 * numNodes

          wattPerNode = wattPerPU + wattPerMem
          costPerNode = costPerPU + costPerMem
          
          costPerPU =
            ite (tflopsPerPU .<= 3) (tflopsPerPU * 1e4 + 7700) $
            ite (tflopsPerPU .<= 9) (tflopsPerPU * 4e4) $
                                    (tflopsPerPU * 8e4)
          wattPerPU =                           
            ite (tflopsPerPU .<= 3) (125 * tflopsPerPU) $
            ite (tflopsPerPU .<= 9) ( 17 * tflopsPerPU) $
                                    ( 10 * tflopsPerPU)

          costPerMem =                                     
            ite (gbytesPerMem .<=   1) (  8e5 * gbytesPerMem) $
            ite (gbytesPerMem .<= 128) (3.2e3 * gbytesPerMem) $
                                       (  100 * gbytesPerMem) 

          gbpsPerMem =                                     
            ite (gbytesPerMem .<=   1) (  1e3 / gbytesPerMem) $
            ite (gbytesPerMem .<= 128) (    1) $
                                       (  0.3) 

          wattPerMem =                                     
            ite (gbytesPerMem .<=   1) (   4 * gbytesPerMem) $
            ite (gbytesPerMem .<= 128) (0.32 * gbytesPerMem) $
                                       (0.08 * gbytesPerMem) 

          runningYear = 10
          runningCostPerYear = 
            auxPowerGW*1e6 * 8{-yen/kWh-} *24*365 
                                       
--       -- task A                                       
--       constrain $ totalFlops .>=  0.2 * 10^18
--       constrain $ totalMem   .>= 40   * 10^15
--       constrain $ totalMemBW .>= 40   * 10^15
--  
--       -- task B                                      
--       constrain $ totalFlops .>=   0.1 * 10^18
--       constrain $ totalMem   .>= 100   * 10^15
--       constrain $ totalMemBW .>= 100   * 10^15
--  
--       -- task C                                     
--       constrain $ totalFlops .>=   2   * 10^18
--       constrain $ totalMem   .>=  10   * 10^15
--       constrain $ totalMemBW .>=  10   * 10^15
 
      -- task D                                     
      constrain $ totalFlops .>=   1   * 10^18
      constrain $ totalMem   .>=   0.2 * 10^15
      constrain $ totalMemBW .>= 500   * 10^15



      constrain $ totalWatt .<= 20 * 10^6 + auxPowerGW * 10^9

      constrain $ budget .>= totalCost
      return (true :: SBool)

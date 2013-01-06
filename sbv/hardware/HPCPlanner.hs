import BinarySearch
import Grouping

import Control.Monad
import Control.Concurrent.STM
import Control.Concurrent.STM.TVar
import Data.SBV
import qualified Data.Map as Map
import Data.List (sort,nub,isInfixOf)
import Control.Concurrent.ParallelIO.Global (parallel)
import System.IO.Unsafe

import Text.Printf

sanitize :: String -> String
sanitize = unlines . map sanitizeL . lines
sanitizeL :: String -> String
sanitizeL str 
  | hasPercent = printf "  %s = %f :: SReal" lhs valp   
  | hasSReal   = printf "  %s = %f :: SReal" lhs val        
  | otherwise  = str
  where
    hasPercent = hasSReal && "%" `elem` ws
    hasSReal = "SReal" `elem` ws && "::" `elem` ws
    ws = words str
    wsr = reverse ws
    valp = (read (wsr!!4) :: Double)/ (read (wsr!!2) :: Double)
    val =  (read (wsr!!2) :: Double)
    lhs = unwords$ takeWhile (/= "=") ws

machineMsgs :: TVar [String]
machineMsgs = unsafePerformIO $ newTVarIO []

main = do
  let allPlans = grouping fsReqs
      allGroups = nub $ sort $ concat allPlans
  putStr "possible machines : "      
  putStrLn $ unwords $ map showReqs allGroups

  pricing <- parallel $ map estimateMachine allGroups

  let priceMap = Map.fromList pricing
      priceFunc :: [SpecReq] -> Rational
      priceFunc = (priceMap Map.!)

  xs <- mapM (estimateMachineMsgs priceFunc) allPlans
  let (bestCost, bestPlan) = head $ sort xs
  putStrLn $ "best plan is: " ++ (unwords $ map showReqs bestPlan)
  putStrLn $ "with cost: " ++ showCost bestCost

  ms <- atomically $ readTVar machineMsgs
  writeFile "machineMsgs.txt" $ unlines $ map sanitize $ ms
  
  return ()

showCost :: Rational -> String
showCost x = show (round (x/1e8) :: Int) ++ "e8 yen"

showReqs :: [SpecReq] -> String
showReqs reqs = paren $ unwords $ map reqName $ reqs
  where paren str = "(" ++ str ++ ")"

estimateMachineMsgs :: ([SpecReq] -> Rational)-> [[SpecReq]] -> IO (Rational, [[SpecReq]])
estimateMachineMsgs priceFunc groupedReqs = do
  putStr "Plan: "
  putStr $ unwords $ map showReqs groupedReqs
  let totalCost = sum costs
      costs = map priceFunc groupedReqs
  putStrLn $ "\tcost = " ++ showCost totalCost
  return $ (totalCost, groupedReqs)

estimateMachine :: [SpecReq] -> IO ([SpecReq], Rational)
estimateMachine reqs = do
  cost <- budgetSatProb $ reqs
  putStrLn $ showReqs reqs ++ " = " ++ showCost cost
  let ret = (reqs, cost)
  return ret

budgetSatProb :: [SpecReq] -> IO Rational
budgetSatProb reqs = do
  (cost, res) <- binarySearchWith z3 defaultBSOpts p
  let msg = unlines
          [ showReqs reqs 
          , showCost cost
          , show res ]

  atomically $ modifyTVar machineMsgs (msg:)
  
  return cost
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


      let totalCost  = costPerNode * numNodes + 
                       buildingCost + 
                       runningCostPerYear * runningYear
          totalWatt  = wattPerNode * numNodes 

          totalFlops = tflopsPerPU * 10^12 * numNodes
          totalMem   = gbytesPerMem* 10^9 * numNodes
          totalMemBW = gbpsPerMem  * 10^9 * numNodes

          buildingCost =
            ite (totalWatt .<= 30e6) (100e8)
                                     (200e8 + 4e8 * (totalWatt-20e6)/1e6)

          wattPerNode = wattPerPU + wattPerMem + 10
          costPerNode = costPerPU + costPerMem + 10000
          
          costPerPU =
            ite (tflopsPerPU .<= 3) (tflopsPerPU * 1e4 + 7700) $
            ite (tflopsPerPU .<= 9) (tflopsPerPU * 4e4) $
                                    (80e4)
          wattPerPU =                           
            ite (tflopsPerPU .<= 3) (125 * tflopsPerPU) $
            ite (tflopsPerPU .<= 9) ( 17 * tflopsPerPU) $
                                    ( 1.7* tflopsPerPU)

          costPerMem =                                     
            ite (gbytesPerMem .<=   1) (  8e4 * gbytesPerMem) $
            ite (gbytesPerMem .<= 128) (3.2e3 * gbytesPerMem) $
                                       (  100 * gbytesPerMem) 

          gbpsPerMem =                                     
            ite (gbytesPerMem .<=   1) (  1e4 / gbytesPerMem) $
            ite (gbytesPerMem .<= 128) (  1e3) $
                                       (300) 

          wattPerMem =                                     
            ite (gbytesPerMem .<=   1) (   4 * gbytesPerMem) $
            ite (gbytesPerMem .<= 128) (0.32 * gbytesPerMem) $
                                       (0.08 * gbytesPerMem) 

          runningYear = 10
          runningCostPerYear = 
            totalWatt / 1e3 * 8{-yen/kWh-} *24*365 
                                       

      let innovSW = False                                  
      innovFactorFlops <- sReal "human innovation(CPU)"
      innovFactorMem   <- sReal "human innovation(Mem)"
      innovFactorMemBW <- sReal "human innovation(BW)"

      if innovSW then do
          constrain $ innovFactorFlops .>= 0
          constrain $ innovFactorMem   .>= 0
          constrain $ innovFactorMemBW .>= 0
          constrain $ innovFactorFlops 
                    + innovFactorMem   
                    + innovFactorMemBW .<= 1
          forM reqs $ \req -> do
            constrain $ totalFlops * (1 + 3 * innovFactorFlops) .>= 
              fromRational (reqFlops req) 
            constrain $ totalMem * (1 + 3 * innovFactorMem)  .>= 
              fromRational (reqMem req) 
            constrain $ totalMemBW * (1 + 3 * innovFactorMemBW).>= 
              fromRational (reqMemBW req) 

      else do
        constrain $ innovFactorFlops .== 0
        constrain $ innovFactorMem   .== 0
        constrain $ innovFactorMemBW .== 0
        forM reqs $ \req -> do
          constrain $ totalFlops .>=
            fromRational (reqFlops req) 
          constrain $ totalMem .>=
            fromRational (reqMem req) 
          constrain $ totalMemBW .>=
            fromRational (reqMemBW req) 




      constrain $ budget .>= totalCost


      let examine msg var = do
            x <- sReal msg
            constrain $ x .== var
      examine "PUType" $
            ite (tflopsPerPU .<= 3) (1.0) $
            ite (tflopsPerPU .<= 9) (2.0) $
                                    (3.0)
      examine "MemType" $
            ite (gbytesPerMem .<=   1) (1.0) $
            ite (gbytesPerMem .<= 128) (2.0) $
                                       (3.0)
       
      examine "B/F" (totalMemBW / totalFlops)
      examine "power(MW)" (totalWatt / 1e6)
--      examine "yen/mem" costPerMem
--      examine "W/PU" wattPerPU
--      examine "W/mem" wattPerMem

      examine "init cost(oku yen)" $
          1e-8 * (costPerNode * numNodes + buildingCost)
      examine "running cost(oku yen)" $
          1e-8 * runningCostPerYear * runningYear

      return (true :: SBool)

data SpecReq = 
  SpecReq 
    { reqName :: String
    , reqFlops :: Rational
    , reqMem :: Rational
    , reqMemBW :: Rational
    } deriving (Eq, Ord, Show)

fsReqs :: [SpecReq]
fsReqs = 
  [ 
    SpecReq
    {reqName = "Base", reqFlops = 1e18, reqMem = 10e15, reqMemBW = 100e15 },
    SpecReq
    {reqName = "SoC", reqFlops = 4e18, reqMem =20e12, reqMemBW =16e18 },
    SpecReq
    {reqName = "Acc", reqFlops = 2e18, reqMem =2e15, reqMemBW =2e15 },
    SpecReq
    {reqName = "B/W", reqFlops = 0.15e18, reqMem = 150e15, reqMemBW = 150e15 }
  ]

{-
fsReqs :: [SpecReq]
fsReqs = 
  [ 
    SpecReq
    {reqName = "Base", reqFlops = 1e18, reqMem = 100e15, reqMemBW = 100e15 },
    SpecReq
    {reqName = "SoC", reqFlops = 10e18, reqMem =50e12, reqMemBW =40e18 },
    SpecReq
    {reqName = "Acc", reqFlops = 5e18, reqMem =50e15, reqMemBW =5e15 },
    SpecReq
    {reqName = "B/W", reqFlops = 0.15e18, reqMem = 150e15, reqMemBW = 150e15 }
  ]-}
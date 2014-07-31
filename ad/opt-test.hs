module Main where

import Data.IORef
import Numeric.Optimization.Algorithms.CMAES

f :: IORef Int -> IORef [(Int,Double)] -> [Double] -> IO Double
f tR logR xs = do 
  t <- readIORef tR
  modifyIORef tR succ
  modifyIORef logR ([(t,x)|x<-xs]  ++)
  
  
  return $ sum (map grid xs) + sum (map well xs) + sum (zipWith sep xs (tail xs))
  
grid :: Double -> Double
grid x = 0.01 * (1-cos(x*(2*pi)))

sep ::  Double -> Double -> Double 
sep x y = max 0  $ x - y + 1

well :: Double -> Double
well x = max 0 (negate x) + max 0 (x - (n  - 1))

n :: Double
n = 12 -- fromIntegral $ length xs
    
    
main :: IO ()
main = do
  tR <- newIORef (0::Int)
  plotLogR <- newIORef ([] :: [(Int, Double)])
  xs0 <- run $ (minimizeIO (f tR plotLogR) $ replicate 12 0){sigma0=10}
  print xs0
  print =<< (f tR plotLogR) xs0
  plotLog <- fmap reverse $ readIORef plotLogR
  writeFile "plot.log" $ unlines [ unwords[show t, show v] | (t,v)<-plotLog]
  
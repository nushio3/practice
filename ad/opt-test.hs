module Main where

import Data.IORef
import Numeric.Optimization.Algorithms.CMAES
import System.IO

import Data.Time

f :: IORef Int -> Handle -> [Double] -> IO Double
f tR h xs = do 
  modifyIORef tR succ
  t <- readIORef tR
  -- hPutStr h $ unlines [ unwords[show t, show v] | v <- xs]  
  -- hFlush h
  return $ sum (zipWith sep xs (tail xs))
    + sum (map well xs)
    -- + sum (map grid xs)  
  
  where 
    grid :: Double -> Double
    grid x = 0.001 * (1-cos(x*(2*pi)))
    
    sep ::  Double -> Double -> Double 
    sep x y = max 0  $ x - y + 1
    
    well :: Double -> Double
    well x = max 0 (negate x) + max 0 (x - (n  - 1))
    
    n :: Double
    n = fromIntegral $ length xs
    
    
main :: IO ()
main = mapM_ benchmark $ concat $map (replicate 5) [2..]

benchmark :: Int -> IO ()
benchmark n = do
  t1 <- getCurrentTime
  tR <- newIORef (0::Int)
  h <- openFile "/dev/null" WriteMode
  xs0 <- run $ (minimizeIO (f tR h) $ replicate n 0){sigma0=fromIntegral n}
  print =<< (f tR stdout) xs0
  t2 <- getCurrentTime
  hl <- openFile "benchmark.txt" AppendMode
  hClose h
  hPutStrLn hl $ unwords [show n, show $ diffUTCTime t2 t1]
  hClose hl
  
  
  
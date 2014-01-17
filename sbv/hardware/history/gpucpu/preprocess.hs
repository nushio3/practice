#!/usr/bin/env runhaskell

import Control.Applicative
import Control.Lens
import Control.Monad
import Data.List
import Data.List.Split
import System.Environment

main :: IO ()
main = do
  fns <- getArgs
  strs <- (lines . concat) <$> mapM readFile fns
  
  let 
      strs2 = filter (not . isPrefixOf "#") strs
      xs = map parse $ map (splitOn ",") strs2
            
      parse [x,y,z] = let
           [day,mon,year] = map read $ splitOn "/" z
        in [year+(mon+day/30)/12, read y]
        
  
  mapM_ putStrLn $ map (unwords. map show) $ sort xs

  
#!/usr/bin/env runhaskell

import Control.Monad
import Data.List (isPrefixOf)
import Data.Maybe (catMaybes)
import Data.String.Utils (split)
import Safe (readMay, atMay)
import Text.Printf

readMR :: String -> Maybe (Double, Double)
readMR str = do
  m <- atMay ws 1 >>= readMay
  r <- atMay ws 2 >>= readMay
  return (m / mJPerME, r / rJPerRE)
  where        
    ws = split "," str
    mJPerME = 1 / 317.828133
    rJPerRE = 6371 / 69911 -- mean radii, (2a+b)/3


main :: IO ()
main = do
  str <- readFile "exoplanet.csv"
  let pcat :: [String]
      pcat = 
        filter (not . isPrefixOf "#") $
        lines str
      mrs = catMaybes $ map readMR pcat
  forM_ mrs $ \(m,r) ->
    printf "%f %f\n" m r
    
  
  
  
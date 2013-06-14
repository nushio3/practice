#!/usr/bin/env runhaskell


main :: IO ()
main = do
  str <- readFile "exoplanet.csv"
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import EmbedExpr

taxOf :: Double -> Double
taxOf = (*0.08)

main :: IO ()
main = do
  let itemList :: [String]
      itemList = ["Apple", "Banana", "Cherry"]
      price :: Double
      price = 350
  putStrLn [myQ|The price for {itemList !! 0} is {price + taxOf price}.|]
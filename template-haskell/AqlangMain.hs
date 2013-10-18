{-# LANGUAGE TemplateHaskell #-}

import Aqlang

xs :: String
xs = [doc| Hello if case as nyan! |]

main :: IO ()
main = do
  putStrLn xs
  
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}

import Aqlang


main :: IO ()
main = do
  let nyan = "28-28-"
  putStrLn [doc| Hello if case as `nyan ! |]
  
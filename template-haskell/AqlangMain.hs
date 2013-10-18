{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}


import Aqlang

main :: IO ()
main = do
  let nyanya = putStr "28-28-"
      
  [rawQ| Ascat is an aznyan. She sais #{nyan} . She sais @{nyanya} ! She's such a cutie^^~~~ |]
       
  where
    nyan :: Integer
    nyan = 28
  
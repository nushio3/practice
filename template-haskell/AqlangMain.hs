{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}


import Aqlang

main :: IO ()
main = do
  let nyanya = "28-28-"
      
  putStrLn [rawQ| Ascat is an aznyan. She sais `nyan . She sais `nyanya ! She's such a cutie^^~~~ |]
  
--   parseTest parseLang "#{AZcat}"
--   parseTest parseLang "#{AZcat}@{BZdog}"
--   parseTest parseLang "#{AZcat}@{BZdog} hoge"
--   parseTest parseLang "kinow #{AZcat}@{BZdog} "
--   parseTest parseLang "Ascat is an aznyan. She sais #{nyan} . She sais #@{nyanya} ! She's such #{a cutie^^~~~"
  
       
  where
    nyan :: Integer
    nyan = 28
  
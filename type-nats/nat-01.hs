{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

import GHC.TypeLits

main = do
  print (sing :: Sing 3)
  print (sing :: Sing (8*9))  
  print (sing :: Sing (2^100))  
  
  
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

import GHC.TypeLits

type family Fib (n :: Nat) :: Nat where
  Fib 0 = 1
  Fib 1 = 1
  Fib n = Fib (n-1) + Fib (n-2)
  
main = do
  print (sing :: Sing 3)
  print (sing :: Sing (8*9))  
  print (sing :: Sing (Fib 10))    
--  print (sing :: Sing (2^100))  
  
  
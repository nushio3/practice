{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

import GHC.TypeLits

type family Fac1 (n :: Nat1) :: Nat where
  Fac1 Zero = 1
  Fac1 (Succ n') = FromNat1 (Succ n') * Fac1 n'

{--

nat-fib-2.hs:10:3:
    Nested type family application
      in the type family application: FromNat1 ('Succ n') * Fac1 n'
    (Use UndecidableInstances to permit this)
    In the equations for closed type family `Fac1'
    In the family declaration for `Fac1'


--}

type family Fac (n :: Nat) :: Nat where
  Fac n = Fac1 (ToNat1 n)
  
main = do
  print (sing :: Sing 3)
  print (sing :: Sing (8*9))  
  print (sing :: Sing (Fac 10))    
  print (sing :: Sing (2^100))  
  
  
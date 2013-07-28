{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

import GHC.TypeLits
class Div3 (n::Nat) (m::Nat) where
instance ((3*m) ~ n) => (Div3 n m) where
pd3 :: forall n m . (SingI n, SingI m, Div3 n m) => Sing n -> IO ()
pd3 _ = do
  print (sing :: Sing m)
main = do
--  pd3 (sing :: Sing (7*8))
  pd3 (sing :: Sing (7*8))

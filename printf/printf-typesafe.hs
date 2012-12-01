{-# LANGUAGE DataKinds #-}
{-# LANGUGAE FlexibleContexts #-}
{-# LANGUGAE FunctionalDependencies #-}
{-# LANGUGAE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeSynonymInstances #-}


import GHC.TypeLits

data Printf (x :: Symbol) = Fmt

class Printer  a b  where
  printf ::  Printf a -> b

instance Printer  "toge" String where
  printf  _ = ""

main = do
  print "b"

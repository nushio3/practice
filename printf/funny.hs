{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}

import GHC.TypeLits

data Printf (x :: Symbol) = Fmt

class Printer a b where
  printf :: Printf a -> b

instance Printer "toge" String where
  printf _ = "echs found!"

main = do
  print "hi"     
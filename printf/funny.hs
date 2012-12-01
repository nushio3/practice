{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

import GHC.TypeLits

data Printf (x :: [Symbol]) = Fmt

class Printer a b  where
  printf :: Printf a -> b

instance Printer '[] String where
  printf  _ = ""

instance forall xs. (Printer xs String) => (Printer ("%f" ': xs) (Double->String)) where
  printf _ x = show x ++ printf (Fmt :: Printf xs)


main = do
  putStrLn $ printf (Fmt :: Printf '["%f"]) (3)

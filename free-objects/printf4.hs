{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE MultiParamTypeClasses#-}
{-# LANGUAGE UndecidableInstances #-}

-- import qualified Data.Array.Repa as R
-- import qualified Data.Array.Repa.Eval as R

run :: PType a t => a -> t
run a0 = spr a0

class PType a t where
  spr :: a -> t

-- instance PType a ((a->b)->b) where
--   spr x f = f x

instance (Show a) => PType a String where
  spr = show


instance (PType (b,a) r) => PType a (b->r) where
  spr a0 = (\b0 -> spr (b0,a0))


main = do
  putStrLn $ run (3::Int) 'c' () (6::Double)

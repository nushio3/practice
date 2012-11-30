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

instance PType Int String where
  spr = show

instance (PType a r) => PType (b,a) (b->r) where
  spr (b0,a0) = (\b0 -> spr a0)


main = do
  putStrLn $ run ('c',(7::Double,5::Int)) 'a' (6::Double)

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE MultiParamTypeClasses#-}
{-# LANGUAGE UndecidableInstances #-}

-- import qualified Data.Array.Repa as R
-- import qualified Data.Array.Repa.Eval as R

run :: PType () t => t
run = spr ()

class PType a t where
  spr :: a -> t

instance PType a ((a->b)->b) where
   spr x f = f x

-- instance (Show a) => PType a String where
--   spr = show


instance (PType (a,b) r) => PType a (b->r) where
  spr a0 = (\b0 -> spr (a0,b0))


main = do
  putStrLn $ run 'c' (3::Int) go
  putStrLn $ run (6::Double) 'c' (3::Int) goer
    where
      go :: (((),Char), Int) -> String
      go xs = show xs

      goer :: ((((),Double),Char), Int) -> String
      goer xs = show xs

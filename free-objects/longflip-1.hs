{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE MultiParamTypeClasses#-}
{-# LANGUAGE UndecidableInstances #-}

run :: PType () t => t
run = spr ()

class PType a t where
  spr :: a -> t

instance PType a ((a->b)->b) where
   spr x f = f x

instance (PType (a->s) r) => PType s (a->r) where
  spr s0 = (\a0 -> spr (a0,b0))


main = do
  print "hi"
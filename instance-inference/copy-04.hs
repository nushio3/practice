{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE UndecidableInstances #-}

class Copyable v a w b | v a -> w b where
  copy :: v a -> w b

class Reduce v a w b| v a -> w b where
  reduce :: v a -> w b

instance Reduce Maybe a [] a where
  reduce (Just x) = [x]
  reduce Nothing  = []

instance Copyable [] Char [] () where
  copy x = [()]

instance (Copyable w b w (), Reduce v a w b) => Copyable v a w () where
  copy = copy . reduce

main = do
  print "hi"
  print $ copy $ Just 'i'

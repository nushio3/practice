{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE UndecidableInstances #-}

class Copyable v a b | v a -> b where
  copy :: v a -> v b

class Reduce a b | a -> b where
  reduce :: a -> b

instance Copyable [] Char () where
  copy x = [()]

instance (Copyable [] a (), Reduce [a] a) => Copyable [] [a] () where
  copy x = [()]

main = do
  print "hi"
  print $ copy [["hi"]]

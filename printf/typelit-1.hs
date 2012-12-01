{-# LANGUAGE DataKinds, FlexibleInstances, FunctionalDependencies, KindSignatures, MultiParamTypeClasses #-}

import GHC.TypeLits

data Label (l :: Symbol) = Get

class Has a l b | a l -> b where
  from :: a -> Label l -> b

data Point = Point Int Int deriving Show

instance Has Point "x" Int where from (Point x _) _ = x
instance Has Point "y" Int where from (Point _ y) _ = y

example :: Int
example = from (Point 1 2) (Get :: Label "x")

main = do
  print example
  print $ from (Point 10 20) (Get :: Label "x")
  print $ from (Point 88 99) (Get :: Label "y")
--  print $ from (Point 88 99) (Get :: Label "z")

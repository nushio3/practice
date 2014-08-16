{-# LANGUAGE FlexibleInstances, GeneralizedNewtypeDeriving, RankNTypes, StandaloneDeriving #-}

import Text.Printf

newtype E a = E {runE :: a}  
  
type S = String

class Math n where
  switch :: f S -> (forall a. f (E a)) -> f n

instance Math S where
  switch x _ = x
instance Math (E a) where
  switch _ x = x
  
deriving instance (Num a) => Num (E a)

instance Num S where
  a + b = printf "%s+%s" a b
  (-)    = undefined
  (*)    = undefined
  abs    = undefined
  signum = undefined
  fromInteger = show

paren :: Math a => a -> a
paren = switch (++")") id

main :: IO ()
main = do
  putStrLn "hi"
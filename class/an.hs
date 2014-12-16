{-# LANGUAGE FlexibleInstances #-}
import Control.Applicative
instance (Applicative a, Num n) => Num (a n) where
  (+) = liftA2 (+)
  (*) = liftA2 (*)   
  
main = do
  print $ [1,2,3] * [4,5,6]
  print $ Just 19 + Just 23

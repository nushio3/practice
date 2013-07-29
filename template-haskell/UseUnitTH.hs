{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

import UnitTH


x :: [ut| erg / cm3 s rad Hz |] Int
x = [ut|  erg / cm3 s rad Hz |] 3

type Two f a = f (f a)
type Three f a = f (f (f a))

main :: IO ()
main = do
  print x
  let y :: Three Maybe (Three Maybe Int)
      y = Nothing
      z :: Two Maybe (Two Maybe (Two Maybe Int))
      z = Just Nothing
  print [Just y,z]

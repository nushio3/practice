{-# LANGUAGE TypeFamilies, MultiParamTypeClasses #-}

import Text.Printf

class Pluggable a b where
  type Result a b :: *
  plug :: a -> b -> Result a b
  
instance Pluggable Int Bool where
  type Result Int Bool = String
  plug x y 
    | y    = printf "yay!%d" x
    | True = printf "noh!%d" x

instance Pluggable Double Char where
  type Result Double Char = String
  plug x y = y:show x

main :: IO ()
main = do
  putStrLn $ plug (3::Int) True
  putStrLn $ plug (pi::Double) '*' 
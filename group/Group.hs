-- import Prelude hiding (*)

module Group where

-----------------------------------------------------
-- General Group type class
-----------------------------------------------------
class (Eq a) => Group a where
  unit :: a
  (*) :: a -> a -> a
  inv :: a -> a
  pow :: a -> Int -> a
  x `pow` n  | (n >= 0)  = foldl (Group.*) unit $ replicate n x
             | otherwise = foldl (Group.*) unit $ replicate (-n) (inv x)

main = do
  print "hi"

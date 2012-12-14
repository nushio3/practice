import Control.Monad
import Data.SBV

main = do
  sol <- minimize (Iterative True) 
   (\[x,y] -> costFunction x y)
   2
   (\[x,y] -> constraintFunction x y)
  print sol
-- seemingly finds x==199, y==1, mincost=3981
-- which is not optimal.

costFunction :: SReal ->  SReal -> SReal
costFunction x y = 
  20*x +y^2

constraintFunction :: SReal ->  SReal ->  SBool
constraintFunction x y = bAnd
  [ x .>= 0,
    y .>= 0,
    x+y .>=200
  ]
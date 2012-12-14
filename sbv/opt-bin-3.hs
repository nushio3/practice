import Control.Monad
import Data.SBV

main2 = do
  sol <- minimize (Iterative True) 
   (\[x,y] -> costFunction x y)
   2
   (\[x,y] -> constraintFunction x y)
  print sol
-- seemingly finds x==199, y==1, mincost=3981

main = do
  (print =<<) $ sat $ forSome ["x","y"] $
    \x y -> do
      constrain $ constraintFunction x y
      return $ costFunction x y .<= 3899


unarySearch :: (SReal -> Predicate) -> Rational -> IO ()
unarySearch pred x = do
  (print =<<) $ sat $ pred x


costFunction :: SReal ->  SReal -> SReal
costFunction x y = 
  20*x +y^2

constraintFunction :: SReal ->  SReal ->  SBool
constraintFunction x y = bAnd
  [ x .>= 0,
    y .>= 0,
    x+y .>=200
  ]
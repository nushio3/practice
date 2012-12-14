import Control.Monad
import Data.SBV

-- seemingly found a boundary.
main = do
  (print =<<) $ sat $ forSome ["x","y"] $
    \x y -> do
      constrain $ constraintFunction x y
      return $ costFunction x y .<= 3900

  (print =<<) $ sat $ forSome ["x","y"] $
    \x y -> do
      constrain $ constraintFunction x y
      return $ costFunction x y .< 3900



costFunction :: SReal ->  SReal -> SReal
costFunction x y =
  20*x +y^2

constraintFunction :: SReal ->  SReal ->  SBool
constraintFunction x y = bAnd
  [ x .>= 0,
    y .>= 0,
    x+y .>=200
  ]
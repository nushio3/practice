import Control.Monad
import Data.SBV

main = do
  let pred thre = forSome ["x","y","z"] $
        \x y z -> do
           constrain $ constraintFunction x y z
           return $ costFunction x y z .<= thre
  tUpper <- unarySearch pred True 1
  tLower <- unarySearch pred False (-1)
  binarySearch pred tLower tUpper


unarySearch :: (SReal -> Predicate) -> Bool -> Rational -> IO Rational
unarySearch pred mode x = do
  res@(SatResult ans) <- sat $ pred (fromRational x)
  putStrLn $  "threashold = " 
           ++ show (fromRational x::Double)
           ++ " : " ++ show res
  case ans of
    Satisfiable _ _ | mode == True  -> return x
    Unsatisfiable _ | mode == False -> return x
    _               -> unarySearch pred mode (2*x)


binarySearch :: (SReal -> Predicate) -> Rational -> Rational -> IO Rational
binarySearch pred tLower tUpper = do
  res@(SatResult ans) <- sat $ pred (fromRational tMid)
  putStrLn $  "threashold = " 
           ++ show (fromRational tMid::Double)
           ++ " : " ++ show res
  case ans of
    _ | (tUpper-tLower<1e-20) -> quit
    Satisfiable _ _ -> binarySearch pred tLower tMid
    Unsatisfiable _ -> binarySearch pred tMid tUpper
    _               -> quit
  where
    tMid = (tLower + tUpper) / 2
    quit = do
      putStrLn "search ends here."
      return tMid
      
  



costFunction :: SReal ->  SReal ->  SReal -> SReal
costFunction x y z = 
  ite (x^2 .>= y^3) posi nega
  where
    posi = x^4 + z
    nega = y^4 + z

constraintFunction :: SReal ->  SReal ->  SReal ->  SBool
constraintFunction x y z = bAnd
  [ x .>= 0,
    y .>= 0,
    z .>= 0,
    x+y+z .>= 10
  ]
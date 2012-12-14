import Control.Monad
import Data.SBV

main = do
  let pred thre = forSome ["x","y"] $
        \x y -> do
           constrain $ constraintFunction x y
           return $ costFunction x y .<= thre
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





costFunction :: SReal ->  SReal -> SReal
costFunction x y = x + y

constraintFunction :: SReal ->  SReal ->  SBool
constraintFunction x y = bAnd
  [ x^2+y^2 .<= 1
  ]
  where

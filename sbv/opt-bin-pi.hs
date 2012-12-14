import Control.Monad
import Data.SBV

main = do
  let pred thre = forSome ["x","p"] $
        \x p -> do
           constrain =<< constraintPred x p
           return $ costFunction x p .<= thre
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





costFunction :: SReal -> SReal -> SReal
costFunction x p = p

constraintPred :: SReal -> SReal -> Predicate
constraintPred x p = do
  constrain $ p.>=0
  constrain $ p^4/90 .>= x
  forAll ["n"] $ \n -> termSum n .<= x
    where
      term :: SInteger -> SReal
      term n = 1/ (toSReal $ n^4)
      termSum :: SInteger -> SReal
      termSum n = ite (n .<= 0) 0 (termSum (n-1) + term n)
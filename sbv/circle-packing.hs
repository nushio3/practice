import Control.Monad
import Data.SBV
import System.Environment

main = do
  str <- getArgs

  let
    n :: Int
    n = read $ head str
    pred thre = do
      xs <- sReals ["x" ++ show i | i<-[0..n-1]]
      ys <- sReals ["y" ++ show i | i<-[0..n-1]]
      let x i = xs!!i
          y i = ys!!i
          r :: Int -> SReal
          r i = fromIntegral (i+1)
      sequence [constrain $ thre .>= r i | i <- [0..n-1]]
      sequence [constrain $ (x i)^2 + (y i)^2 .<= (thre-(r i))^2
               | i <- [0..n-1]]
      sequence [constrain $ (x i - x j)^2 + (y i - y j)^2
                            .>= (r i + r j)^2
               | i <- [0..n-1], j <- [i+1..n-1]]
      return (true :: SBool)
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

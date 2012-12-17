import Control.Monad
import Data.SBV
import System.Environment

main = do
  str <- getArgs

  let
    n :: Int
    n = read $ head str
    pred :: SInteger -> Predicate
    pred thre = do
      xs <- sIntegers ["x" ++ show i | i<-[0..n-1]]
      ys <- sIntegers ["y" ++ show i | i<-[0..n-1]]
      let x i = xs!!i
          y i = ys!!i
          r :: Int -> SInteger
          r i = fromIntegral ((100)*i+1)
      sequence [constrain $ thre .>= (r i)| i <- [0..n-1]]
      sequence [constrain $ ((x i)^2 + (y i)^2)
                         .<= (thre-(r i))^2
               | i <- [0..n-1]]
      sequence [constrain $ (x i - x j)^2 + (y i - y j)^2
                            .>= (r i + r j)^2
               | i <- [0..n-1], j <- [i+1..n-1]]
      return (true :: SBool)
--   tUpper <- unarySearch pred True 10
--   tLower <- unarySearch pred False (-1)
--   binarySearch pred tLower tUpper
  minimize (Iterative True) cost 7 valid
    where
      cost = head
      valid [t,ax,ay,bx,by,cx,cy] = bAnd
        [ t .>= (300::SInteger)
        , (t-100)^2 .>= ax^2+ay^2
        , (t-200)^2 .>= bx^2+by^2
        , (t-300)^2 .>= cx^2+cy^2
        , (ax-bx)^2+(ay-by)^2 .>= 300^2
        , (ax-cx)^2+(ay-cy)^2 .>= 400^2
        , (bx-cx)^2+(by-cy)^2 .>= 500^2
          ]


unarySearch :: (SInteger -> Predicate) -> Bool -> Integer -> IO Integer
unarySearch pred mode x = do
  res@(SatResult ans) <- sat $ pred (fromInteger x)
  putStrLn $  "threashold = "
           ++ show x
           ++ " : " ++ show res
  case ans of
    Satisfiable _ _ | mode == True  -> return x
    Unsatisfiable _ | mode == False -> return x
    _               -> unarySearch pred mode (2*x)


binarySearch :: (SInteger -> Predicate) -> Integer -> Integer -> IO Integer
binarySearch pred tLower tUpper = do
  res@(SatResult ans) <- sat $ pred (fromInteger tMid)
  putStrLn $  "threashold = "
           ++ show tMid
           ++ " : " ++ show res
  case ans of
    _ | (tUpper==tLower) -> quit
    Satisfiable _ _ -> binarySearch pred tLower tMid
    Unsatisfiable _ -> binarySearch pred tMid tUpper
    _               -> quit
  where
    tMid = (tLower + tUpper) `div` 2
    quit = do
      putStrLn "search ends here."
      return tMid

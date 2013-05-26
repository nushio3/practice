import Control.Monad
import Data.List (isInfixOf, elem)
import Data.SBV
import Text.Printf

globalNum :: Int
globalNum = 43

predicate :: SReal -> Predicate
predicate thre = do
  xs <- sequence [sReal $ printf "x%02d" i | i <- [0..globalNum-1]]
  vs <- sequence [sReal $ printf "v%02d" i | i <- [0..globalNum-2]]
  as <- sequence [sReal $ printf "a%02d" i | i <- [0..globalNum-1]]
  let asap :: SReal -> Symbolic ()
      asap x = constrain $ x^2 .<= thre
      dt :: SReal
      dt = 6.28 / fromIntegral globalNum
  mapM_ asap [(xs!!i - xs!!(i+1))/dt - vs!!i   | i <- [0..globalNum-2]]
  mapM_ asap [(vs!!i - vs!!(i+1))/dt - as!!i   | i <- [0..globalNum-3]]
  mapM_ asap [xs!!i + as!!i  | i <- [0..globalNum-1]]
  return $ (xs!!0 .== 1) &&& (xs!!(globalNum-1) .== 1)

main = do
  tUpper <- unarySearch predicate True 1
  tLower <- unarySearch predicate False (-1)
  binarySearch predicate tLower tUpper


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
           ++ " : " ++ showRes res
  case ans of
    _ | (tUpper-tLower<1e-20) -> quit
    Satisfiable _ _ -> binarySearch pred tLower tMid
--    Unsatisfiable _ -> binarySearch pred tMid tUpper
    _               -> binarySearch pred tMid tUpper
--    _               -> quit
  where
    tMid = (tLower + tUpper) / 2
    quit = do
      putStrLn "search ends here."
      return tMid

    showRes :: Show a => a -> String
    showRes x =
      unlines $
      map (\str -> if '%' `elem` str then unp str else str) $
      lines $ show x

    unp :: String -> String
    unp str = let ws = words str in
      printf "%s %s %f" (ws!!0) (ws!!1)
      ((read$ws!!2) / (read $ ws!!4) :: Double)

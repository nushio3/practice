import Control.Monad
import Data.SBV
import Text.Printf
import Data.List

sz :: Int
sz = 16

lim :: Int
lim = 2

prob :: Predicate
prob = do
  tblVars <- sequence [
    sequence [ sBool $ printf "(%d,%d)" x y | x<-[0..sz-1]] | y<-[0..sz-1]]

  let notsame :: [SBool] -> SBool
      notsame xs = bOr xs &&& bOr (map bnot xs)

      tbl (x,y) = tblVars !! y !! x

      cnsv x y = constrain $ notsame [ tbl (x,y+d) | d <- [0..lim]]
      cnsh x y = constrain $ notsame [ tbl (x+d,y) | d <- [0..lim]]
      cnsd1 x y = constrain $ notsame [ tbl (x+d,y+d) | d <- [0..lim]]
      cnsd2 x y = constrain $ notsame [ tbl (x+lim-d,y+d) | d <- [0..lim]]

  sequence [cnsv x y | x <- [0..sz-1], y <- [0..sz-lim-1]]
  sequence [cnsh x y | x <- [0..sz-lim-1], y <- [0..sz-1]]
  sequence [cnsd1 x y | x <- [0..sz-lim-1], y <- [0..sz-lim-1]]
  sequence [cnsd2 x y | x <- [0..sz-lim-1], y <- [0..sz-lim-1]]
  constrain $ tbl (0,0)
  constrain $ bnot $ tbl (15,15)
  return (true :: SBool)


main2 = do
  sol <- allSat prob
  print sol

main = do
  sol <- sat prob
  let oracle :: [String]
      oracle = lines $ show sol
            
      ans :: Int -> Int -> Bool
      ans x y = 
        read $ last $ words $ head $
        filter (isInfixOf $ show (x,y)) $
        oracle
  case "Unsat" `isInfixOf` show sol of
    True -> print sol
    _    ->
      mapM_ putStrLn 
        [ [ if ans x y then 'O' else 'X' |
            x <- [0..sz-1]] | y <- [0..sz-1]]
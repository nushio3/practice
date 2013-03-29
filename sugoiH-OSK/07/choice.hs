#!/usr/bin/env runhaskell

import Safe (readMay)
import Text.Printf

choice :: [(String, a)] -> IO a
choice xs = do
  let msgs = zipWith (printf "(%d) %s") [(1::Int)..] (map fst xs)
      lo = 1
      hi = length xs
      ask = do
        printf "choice(%d-%d)? \n" lo hi
        input <- getLine
        case readMay input of
          Just n | (lo<=n && n<=hi) -> return $ snd $ xs!!(n-1)
          _                         -> ask
  putStr $ unlines msgs
  ask

main = do
  next <- choice [ ("寿司を食べる"   , sushi)
                 , ("名前を入力する" , name) ]
  next           

name = do
  putStrLn "苗字？"
  famN <- getLine
  putStrLn "名前？"
  givN <- getLine
  putStrLn "フォーマット？"
  fullNameGen <- choice 
    [ ("苗字・名前", return $ printf "%s・%s" famN givN)
    , ("名前・ミドルネーム＝苗字", do
         putStrLn "ミドルネーム？"
         midN <- getLine
         return $ printf "%s・%s＝%s" givN midN famN
           )]
  fullName <- fullNameGen


  printf "ドーモ,%sサン。私はヌシオです。\n" (fullName :: String)



sushi = do
  x <- choice [ ("海鮮丼"   ,  980) 
              , ("味噌汁"   ,  200) 
              , ("いくら丼" , 3800) ]
  printf "%d円になります\n" (x :: Int)
  

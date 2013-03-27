#!/usr/bin/env runhaskell

import Safe (readMay)
import Text.Printf

choice :: [(String, IO a)] -> IO a
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
  m <- ask
  m

main :: IO ()
main = top 10

top :: Int -> IO ()
top mp = do
  putStrLn $ "あなたのMPは:" ++ show mp
  putStrLn "どこへ行く？"
  choice [ ("宿屋" , inn)
         , ("野外" , outdoor mp) ]

inn = do
  putStrLn "あなたはぐっすり眠った。"
  top 10

outdoor mp = do
  putStrLn "マネジメント上の問題が現れた!!どうする？"
  choice [ ("殴る" , fight)
         , ("メラ" , magic 3 mp) 
         , ("イオナズン", magic 100 mp) ]

fight = do
  putStrLn "＿人人人人人人人＿"
  putStrLn "＞ 突然の突き指 ＜"
  putStrLn "￣Y^Y^Y^Y^Y^Y^Y￣"
  putStrLn "あなたは死にました・・・"

magic costMp mp 
 | costMp > mp = do
     putStrLn "MPが足りない"　
     putStrLn "あなたは死にました・・・"
 | otherwise = do
     putStrLn "炎上マーケティング！！"
     putStrLn "マネジメント上の問題を解決した"
     top (mp - costMp)

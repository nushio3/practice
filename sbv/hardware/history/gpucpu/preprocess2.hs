#!/usr/bin/env runhaskell

import Control.Applicative
import Control.Lens
import Control.Monad
import Data.List
import Data.List.Split
import Text.Printf
import System.Environment

every :: Int -> [a] -> [[a]]
every n xs = case splitAt n xs of
  (hs,[]) -> [hs]
  (hs,ts) -> hs: every n ts

data Bench = Bench 
  {year :: Double,
   flops:: Double,
   name :: String}deriving(Ord,Eq,Show)

main :: IO ()
main = do
  fns <- getArgs
  strs <- (lines . concat) <$> mapM readFile fns
  

  let pcs =  every 14 strs
      parse xs = Bench 
                   (parseMY $ filter isDigit $ xs!!3)
                   (read $ filter isDigit $ xs!!9)
                   (parseN $ xs!!7)
      parseMY :: String -> Double
      parseMY = (\(x, y) -> read y + read x/12). splitAt 2

      isDigit :: Char -> Bool
      isDigit = flip elem "0123456789."


      ppr :: Bench -> String
      ppr (Bench y p _) = printf "%.2f %f" y p

      fixp :: Bench -> Bench
      fixp (Bench y p n) = Bench y (if y > 2005 then 1e12*p else 1e9*p) n
      
      futuredat = 
        [ Bench 2018 1e18 "US Exa" 
        , Bench 2020 1e18 "Japan Exa" ]
      dat = filterUniqueN ""$ sort$futuredat ++ map (fixp.parse) pcs


  mapM_ putStrLn $ map ppr dat
  writeFile "top500-labels.txt" $ unlines $ map pprl dat


pprl (Bench y p n) = printf "set label '%s'  at %f, %f left font 'Arial,15' rotate by 315" n (y+0.5) (p*0.7)

parseN str = if parseN3 str =="" then parseN2 str else parseN3 str

parseN3 :: String -> String
parseN3 = 
  takeWhile (\c -> c/='(' && c/='<'&& c/=',') .
  drop 1 .
  dropWhile (/='>') . drop 1.dropWhile (/='>'). drop 1 . dropWhile (/='>')
  

parseN2 :: String -> String
parseN2 = 
  takeWhile (\c -> c/='(' && c/='<'&& c/=',') .
  drop 1 .
  dropWhile (/='>') . drop 1.dropWhile (/='>')
  

filterUniqueN str [] = []
filterUniqueN str ((b@(Bench y p n) : bs)) 
  | str==n = Bench y p "" : filterUniqueN str bs
  | otherwise = b: filterUniqueN n bs   

   
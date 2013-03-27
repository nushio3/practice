#!/usr/bin/env runhaskell

import Text.Printf

charWidth :: Char -> Int
charWidth c
  | c < toEnum 256 = 1
  | otherwise      = 2 
  
strWidth :: String -> Int
strWidth = sum . map charWidth      


main :: IO ()
main = do
  str <- getContents

  let strs = lines $ "突然の" ++ str

      w = maximum $ map strWidth strs
      h = length strs
      
      w2 = (w+1) `div` 2
      
      decor s1 s2 = printf "%s%s%s" 
        s1 (concat $ replicate (w2+2) s2) s1

      pad str = str ++ replicate (w2*2 - strWidth str) ' '

      header = decor "＿" "人"
      footer = decor "￣" "Y^"

      body = map (printf "＞　%s　＜" . pad) strs

  putStrLn $ unlines $
      [header] ++ body ++ [footer]


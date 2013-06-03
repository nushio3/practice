#!/usr/bin/env runhaskell
import System.Environment
{-
Use -pgmF cmd to select the program to use as the preprocessor. When invoked, the cmd pre-processor is given at least three arguments on its command-line: the first argument is the name of the original source file, the second is the name of the file holding the input, and the third is the name of the file where cmd should write its output to.
-}

main :: IO ()
main = do
  argv <- getArgs
  print argv

{-# LANGUAGE GADTs #-}

import Control.Applicative
import Control.Monad.Operational.Mini
import Control.Monad.Operational.Class
import Data.List (isInfixOf)

data Inst x where
  SayHello :: Inst ()
  GetLine :: Inst String
  PutStrLn :: String -> Inst ()



prog :: Program Inst ()
prog = do
  singleton SayHello
  singleton $ PutStrLn "Input a number"
  n <- read <$> singleton GetLine
  singleton $ PutStrLn $ "double of that number is : " ++ show ((2::Int) * n)

main :: IO ()
main = interpret go prog where

  {- interpreter that refuses to say "72" . -}

  go :: Inst x -> IO x
  go SayHello = putStrLn "ohayougozaimasu!"
  go GetLine = getLine
  go (PutStrLn str)
    | "72" `isInfixOf` str = putStrLn "Ku!"
    | otherwise            = putStrLn str

{-

$ ./dist/build/test-01/test-01
ohayougozaimasu!
Input a number
7
double of that number is : 14
$ ./dist/build/test-01/test-01
ohayougozaimasu!
Input a number
21
double of that number is : 42
$ ./dist/build/test-01/test-01
ohayougozaimasu!
Input a number
36
Ku!
$ ./dist/build/test-01/test-01
ohayougozaimasu!
Input a number
8388608
Ku!

-}
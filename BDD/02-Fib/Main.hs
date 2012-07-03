{-# OPTIONS -Wall #-}
import Fib
import System.Environment

main :: IO ()
main = do
  n <- fmap (read . head)getArgs
  print $ fib n

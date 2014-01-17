#!/usr/bin/env runhaskell

import Control.Monad
import Data.List
import Data.List.Split
import System.Environment

main :: IO ()
main = do
  fns <- getArgs
  strs <- concat <$> mapM readFile fns
  putStrLn strs

  
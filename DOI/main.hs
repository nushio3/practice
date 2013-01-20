#!/usr/bin/env runhaskell

import Control.Monad
import System.Process
import System.IO

doi2BibTeX :: String -> IO String
doi2BibTeX doi = do
  let cmd =  "curl -LH 'Accept: text/bibliography; style=bibtex' http://dx.doi.org/" ++ doi
  hPutStrLn stderr cmd
  (_,fp,_,_) <- runInteractiveCommand cmd
  hGetContents fp

main = do
  putStrLn =<< doi2BibTeX "10.1088/0004-637X/744/2/101"

#!/usr/bin/env runhaskell


import Control.Monad
import qualified Data.ByteString.Char8 as BS
import System.Process
import System.IO
import Network.Curl.Download
import Network.Curl.Opts

doi2BibTeX :: String -> IO BS.ByteString
doi2BibTeX doi = do
  let 
      opts = [ CurlFollowLocation True
             , CurlHttpHeaders ["Accept: text/bibliography; style=bibtex"]
             ]
      url = "http://dx.doi.org/" ++ doi
  res <- openURIWithOpts opts url
  case res of
    Right bs -> return bs

main = do
  BS.putStrLn =<< doi2BibTeX "10.1088/0004-637X/744/2/101"

module Utility where

import qualified System.IO as IO


hPutStrLnImmediate :: IO.Handle -> String -> IO ()
hPutStrLnImmediate h str = do
   IO.hPutStrLn h str
   IO.hFlush h

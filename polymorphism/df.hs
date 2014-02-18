module Main where

import System.Environment

data Frame = FrameI [Int] | FrameD [Double]
  deriving (Show)

readFrame :: FilePath -> IO Frame
readFrame fn = do
  str <- readFile fn
  let (hl:tl) = lines str
  return $ case hl of
    ('I':_) -> FrameI $ reader tl
    ('D':_) -> FrameD $ reader tl
  
  where 
    reader :: Read a => [String] -> [a]
    reader = map read

main :: IO ()
main = do
  [fn] <- getArgs
  readFrame fn >>= print
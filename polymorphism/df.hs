module Main where

import System.Environment

data Frame = FrameI [Int] | FrameD [Double]
  deriving (Show)

readFrame :: FilePath -> IO Frame
readFrame fn = do
  str <- readFile fn
  let (hl:tl) = lines str
  return $ case hl of
    ('I':_) -> FrameI $ map read tl
    ('D':_) -> FrameD $ map read tl

main :: IO ()
main = do
  [fn] <- getArgs
  readFrame fn >>= print
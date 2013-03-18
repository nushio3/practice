module Main where

import Codec


main :: IO ()
main = do
  let ans :: ([String],Either () Double)
      ans = (["Life", "Universe", "Everything"], Right 4.2)
  testCodec codecBinary ans
  testCodec codecShowRead ans
  testCodec codecJSON ans
  testCodec codecYaml ans
  testCodec codecDynamic ans

{-# LANGUAGE OverloadedStrings #-}

import Crypto.BCrypt
import qualified Data.ByteString.Char8 as BS
import Control.Concurrent.ParallelIO.Global (parallel)

main = do
  xs <- parallel $ map encode [1..100]
  print $ and xs


encode :: Integer -> IO Bool
encode n = do
  let xs = BS.pack $ show $ (2::Integer)^n
  Just ys <- hashPasswordUsingPolicy slowerBcryptHashingPolicy $ xs
  let b = validatePassword ys xs
  b `seq` return b

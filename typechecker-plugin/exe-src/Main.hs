{-# OPTIONS_GHC -fplugin MyPlugin #-}

data A = A deriving (Show)

data B = B

main :: IO ()
main = print B

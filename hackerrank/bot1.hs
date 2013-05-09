{-# LANGUAGE TupleSections #-}
module Main where

getList :: Int -> IO [String]
getList n = if n==0 then return [] else do i <- getLine; is <- getList(n-1); return (i:is)
-- Head ends here
displayPathtoPrincess :: Int -> [String] -> [String]
displayPathtoPrincess _ strs = ["LEFT","UP","DOWN","RIGHT"]
  where
    ps = zipWith (\y row -> map (y,) row) [0..] strs


-- Tail starts here
main = do
   m <- getLine
   let i = read m
   grid <- getList i
   mapM_ putStrLn $ displayPathtoPrincess  i  grid

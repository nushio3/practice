module Grouping where

grouping :: [a] -> [[[a]]]
grouping [] = []
grouping [x] = [[[x]]]
grouping (x:xs) = do
  gxs <- grouping xs
  let joinAt i = [(if i==j then (x:) else id) $ gxs!!j | j <- [0..length gxs-1]]
  ([x]:gxs) : [joinAt i | i <- [0..length gxs-1]]
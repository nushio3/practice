main = do
  print $ map (^2) $ filter odd $ [1..5]

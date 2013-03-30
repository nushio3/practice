module Main where


gaussian :: Floating a => [a] -> a
gaussian = exp . negate . sum . map (^2) 

main :: IO ()
main = do
  print $ map gaussian $ map (\x -> [x,x/3]) $ [1..10]

  
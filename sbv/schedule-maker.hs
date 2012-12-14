import System.Random

main = sequence_ $ replicate 30 $ do
  xs <- sequence $ replicate 10 $ randomRIO (1000,2000)
  putStrLn $ unwords $ map show (xs::[Int])
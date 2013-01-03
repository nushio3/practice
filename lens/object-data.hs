
data Object a = Object
  { mass :: a
  , velocity :: a 
  }

momentum :: Num a => Object a -> a
momentum obj = mass obj * velocity obj

x :: Num a => Object a
x = Object 6 7

main = do
  print $ 1000 * momentum x
  print $ 1e41 * momentum x

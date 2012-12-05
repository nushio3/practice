class Copyable v  where
  copy :: v a -> [v a]

instance Copyable [] where
  copy x = [x,x]

main = do
  print "hi"
  print $ copy "hi"

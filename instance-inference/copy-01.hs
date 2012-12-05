#!/usr/bin/env runhaskell
class Copyable v  where
  copy :: v a -> [v a]

instance Copyable [] where
  copy x = [x,x]

main :: IO Int
main = do
  print "hi"
  print $ copy "hi"
  return 3
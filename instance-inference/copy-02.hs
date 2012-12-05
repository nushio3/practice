{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}

class Copyable v a b | v a -> b where
  copy :: v a -> [v b]

instance Copyable [] Char () where
  copy x = [[()]]

main = do
  print "hi"
  print $ copy "hi"

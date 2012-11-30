{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}

run :: PType r => r
run = spr []

class PType t where
  spr :: [String] -> t

instance PType String where
  spr xs = unwords xs

instance (Show a, PType r) => PType (a->r) where
  spr xs = (\x -> spr (show x:xs))


main = do
  putStrLn $ run "hoge" 3 4.5

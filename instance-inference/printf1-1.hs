{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}

printf :: PType r => r
printf = spr []

class PType t where
  spr :: [String] -> t

instance PType String where
  spr xs = unwords $ reverse xs

instance (Show a, PType r) => PType (a->r) where
  spr xs = (\x -> spr (show x:xs))

main :: IO ()
main = do
  putStrLn $ printf 0.1
  putStrLn $ printf 0.1 "U" 178

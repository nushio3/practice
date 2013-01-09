-- File: printf1-2.hs
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

printf0 = printf
printf1 = printf
printf2 = printf
printf3 = printf

main :: IO ()
main = do
  putStrLn $ printf0 
  putStrLn $ printf1 178
  putStrLn $ printf2 'U' 178
  putStrLn $ printf3 0.1 'U' 178

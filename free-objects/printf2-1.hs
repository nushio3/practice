{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}

printf :: PType r => r
printf = spr []

class PType t where
  spr :: [String] -> t

instance PType (IO a) where
  spr xs = do
    putStrLn $ unwords $ reverse xs
    return undefined

instance (Show a, PType r) => PType (a->r) where
  spr xs = (\x -> spr (show x:xs))


main :: IO ()
main = do
  printf
  printf 0.1
  printf 0.1 'U'
  printf 0.1 'U' 178

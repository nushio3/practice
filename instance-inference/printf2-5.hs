{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-}

printf :: PType r => r
printf = spr []

class PType t where
  spr :: [String] -> t

instance PType (IO ()) where
  spr xs = putStrLn $ unwords $ reverse xs

instance (Show a, PType r) => PType (a->r) where
  spr xs = (\x -> spr (show x:xs))

main :: IO ()
main = do
  printf 0.1
  printf "U"

{----
$ runhaskell printf2-1.hs

printf2-1.hs:18:3:
    No instance for (PType (IO a0)) arising from a use of `printf'
    The type variable `a0' is ambiguous
    Possible fix: add a type signature that fixes these type variable(s)
    Note: there is a potential instance available:
      instance PType (IO ()) -- Defined at printf2-1.hs:10:10
    Possible fix: add an instance declaration for (PType (IO a0))
    In a stmt of a 'do' block: printf 0.1
    In the expression:
      do { printf 0.1;
           printf "U" }
    In an equation for `main':
        main
          = do { printf 0.1;
                 printf "U" }
-----}
{-
in Section 11.4 of Haskell 2010 : https://www.haskell.org/onlinereport/haskell2010/haskellch11.html#x18-18600011.4

> It should be the case that
> (x,"") is an element of (readsPrec d (showsPrec d x ""))

-}

data X =  String :☆ String  deriving (Eq, Ord, Show, Read)

main :: IO ()
main = do
  let x = "" :☆ ""
      d = 0
  print $ map fromEnum $ show x
  print $ (x, "") `elem` (readsPrec d (showsPrec d x ""))

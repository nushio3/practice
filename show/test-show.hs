{-
in Section 11.4 of Haskell 2010 : https://www.haskell.org/onlinereport/haskell2010/haskellch11.html#x18-18600011.4

> It should be the case that
> (x,"") is an element of (readsPrec d (showsPrec d x ""))

-}
data Y = Д星 String  String  deriving (Eq, Ord, Show, Read)

data X = String :++ String  deriving (Eq, Ord, Show, Read)


main :: IO ()
main = do
  let y = Д星 "" ""
      d = 0
  print $ map fromEnum $ show y
  print $ (y, "") `elem` (readsPrec d (showsPrec d y ""))
  print $ (read $ show y :: Y)

  let x = "" :++ ""
      d = 0
  print $ map fromEnum $ show x
  print $ (x, "") `elem` (readsPrec d (showsPrec d x ""))
  print $ (read $ show x :: X)

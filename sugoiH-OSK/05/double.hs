

type Doppel f x = f (f x)

xss :: Doppel [] Int
xss = [[1,2],[3],[]]

ymm :: Doppel Maybe String
ymm = Just (Just "hey")

main = do
     print xss
     print ymm

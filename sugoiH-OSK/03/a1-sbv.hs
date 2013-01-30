import Data.SBV

type E = SWord8

sFilter :: (E -> SBool) -> [E] -> [E]
sFilter f [] = []
sFilter f (x:xs) = ite (f x) (x:sFilter f xs)  (sFilter f xs)

main = do
  ret <- correctness 5
  print ret

sEven = (.== 0) . (`sDiv` 2)

correctness :: Int -> IO ThmResult
correctness n = prove $ do
  xs <- mkFreeVars n
  return $ sFilter sEven xs .== [] -- sFilter sEven xs
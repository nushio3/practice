import Data.SBV

type E = SWord8

colLen :: E -> E
colLen n = ite ( n .<= 1) 1 (colLen (col n))

col :: E -> E
col n = ite (sEven n) (n `sDiv` 2) (3*n+1)

sEven = (.== 0) . (`sDiv` 2)



main = do
  ret <- sat $ do
    x <- sWord8 "x"
    return $ colLen x .== 3
  print ret
  print "hi"

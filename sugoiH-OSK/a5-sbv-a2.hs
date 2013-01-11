import Data.SBV

type E = SInteger

colLen :: E -> E
colLen n = ite ( n .<= 1) 1 (colLen (col n))

col :: E -> E
col n = ite (sEven n) (n `sMod` 2) (3*n+1)

sEven = (.== 0) . (`sMod` 2)
sOdd = (.== 1) . (`sMod` 2)

limit = 1000000

main = do
  ret <- sat $ do
    xs     <- (newArray "xs" Nothing :: Symbolic (SArray Integer Integer))
    retIdx <- sInteger "retIdx"

    forall "i" >>= (\i -> constrain $ (i.<=1)
                          ==> readArray xs i .== 1)
    forall "i" >>= (\i -> constrain $ (i.>= limit)
                          ==> readArray xs i .== 1)
    forall "i" >>= (\i -> constrain $ (i.>=2 &&& i .< limit &&& sEven i)
                          ==> readArray xs i .== 1+readArray xs (sDiv i 2))
    forall "i" >>= (\i -> constrain $ (i.>=2 &&& i .< limit &&& sOdd i)
                          ==> readArray xs i .== 1+readArray xs (3*i+1))

    return $ (readArray xs retIdx) .== (100)
  print ret
  print "hi"

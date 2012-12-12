import ZipN

tcurry r = uncurryN (undefined::(S (S (S Z)))) (\a b c d -> (((a,b),c),d)) r
-- tcurry :: (((t2, t3), t1), t) -> (((t2, t3), t1), t)





test1 = zipN [1,2,3] (\x -> (x::Int) + 1)
-- [2,3,4]

test2 = zipN [1,2,3] [10,11,12] (\x y -> ((x::Int) + y))
-- [11,13,15]

test3 = zipN [1,2,3] [10,11,12] [100,110,120] (\x y z -> ((x::Int) + y + z))
-- [111,123,135]

main = do
  print test1
  print test2
  print test3
import Data.SBV

minmod3 a b c = ite (a .>0 &&& b .>0 &&& c .>0) (smin a (smin b c)) (ite (a .<0 &&& b .<0 &&& c .<0) (smax a (smax b c)) 0)

normal3 a b c = fpIsNormal a &&& fpIsNormal b &&& fpIsNormal c

main = do
  ret <- prove $ \a b c -> normal3 a b c ==> signum (a::SDouble) * smax 0 (smin (abs a)  (smin (signum a * b) (signum a * c))) .== minmod3 a b c
  print ret


import Data.SBV

main = do
   (print =<<) $ sat $ do
     let a = uninterpret "a"
         b = uninterpret "b"
         c = uninterpret "c"
     x <- exists "x"
     return $ a*x^2+b*x+c.==(0::SReal)

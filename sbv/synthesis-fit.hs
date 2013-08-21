import Control.Monad
import Data.SBV


main :: IO ()
main = do
  ret <- sat $ do
    a <- exists "a"
    b <- exists "b"    
    c <- exists "c"
    ds <- forM [1..10] $ \x' -> do
      let x :: SReal
          x = fromIntegral x'
      return $ (a*x*x+b*x+c-1/x)^2
    return $ sum ds .<= 0.09
  print ret
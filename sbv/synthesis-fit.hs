import Control.Monad
import Data.SBV


main :: IO ()
main = do
  ret <- sat $ do
    a <- exists "a"
    b <- exists "b"    
    c <- exists "c"
    ds <- forM [1..1000] $ \x' -> do
      let x :: SReal
          x = fromIntegral x'
      constrain $ (a*x*x+b*x+c-1/x)^2 .<=0.2
    return $ (true :: SBool)
  print ret
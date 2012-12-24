import Control.Monad
import Data.SBV

targetFunc :: Double -> Double
targetFunc x = sin(x*x/3)

main :: IO ()
main = do
  let
    xs = map ((pi/100)*) [0..100]
    xyds :: [(Double, Double)]
    xyds = [(x, targetFunc x)|x<-xs]
    xys :: [(Rational, Rational)]
    xys = map (\(x,y)->(ap x,ap y)) xyds
    ap x = approxRational x 1e-3
    s (x,y) = unwords [show (fromRational x::Double),
                       show (fromRational y::Double)]
  writeFile "target.txt" $ unlines $ map s xys
  ret <- sat $ do
    a0 <- sReal "a0"
    a1 <- sReal "a1"
    a2 <- sReal "a2"
    a3 <- sReal "a3"
--    a4 <- sReal "a4"
--    a5 <- sReal "a5"
--    a6 <- sReal "a6"
    forM_ xys $ \(rx,ry) -> do
      let x = (fromRational rx)
          y = (fromRational ry)
      constrain $ (a0 + a1 * x + a2 * x^2 + a3 * x^3
                   -- +a4 * x^4 + a5 * x^5 + a6 * x^6
                   - y)^2 .<= 1e-5
    return (true :: SBool)
  print ret

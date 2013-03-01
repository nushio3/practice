
import qualified Numeric.Optimization.Algorithms.CMAES as Opt
import           Control.Concurrent.ParallelIO.Global (parallel)
import           System.IO.Unsafe

main = main1 2

main1 n = do
  let conf = Opt.minimize noise $ xs0
      xs0 = replicate n 0
  putStrLn $ "noise bef = " ++ show (noise xs0)
  xs <- Opt.run $ conf -- {Opt.verbose = True}
  putStrLn $ "noise aft = " ++ show (noise xs)
  putStrLn $ unwords $ map show xs
  main1 $ n+1

noise :: [Double] -> Double
noise args = unsafePerformIO $ do
  ys <- parallel $ map (return . r2n) ranges
  return $ sum ys
-- noise args = sum $  map r2n ranges
  where
    r2n (a, b) =
        (f b - f a - integ a b g)^2 +
        (g b - g a + integ a b f)^2
      where f = poly fargs
            g = poly gargs

    ranges = [let x = fromIntegral i
                  dx = 2*pi / fromIntegral nDiv
              in (x*dx, (x+1)*dx)
             | i<-[0..nDiv]]


    fargs = (1:) $ map snd $ filter (even . fst) $ zip [0..] args
    gargs = (0:) $ map snd $ filter (odd  . fst) $ zip [0..] args

poly :: [Double] -> Double -> Double
poly as x = sum $ zipWith (\a p -> a * x^p) as [0..]


nDiv :: Int
nDiv = 1000

nIntegral :: Int
nIntegral = 1000

integ :: Double ->  Double -> ( Double -> Double ) -> Double
integ bot top f =
  let dx = (top-bot) / fromIntegral nIntegral in
    sum $
    map (\(w,x) -> w * dx * f x) $
    map (\i -> (if i==0 || i==nIntegral then 0.5 else 1,
                bot + dx * fromIntegral i)) $
    [0..nIntegral]
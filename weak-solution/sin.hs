import qualified Numeric.Optimization.Algorithms.CMAES as Opt

main = main1 2

main1 n = do
  let conf = Opt.minimize noise $ replicate n 0
  xs <- Opt.run $ conf -- {Opt.verbose = True}
  putStrLn $ "noise = " ++ show (noise xs)
  putStrLn $ unwords $ map show xs
  main1 $ n+1

noise :: [Double] -> Double
noise args = sum $ map r2n ranges
  where
    r2n (a, b) =
        (f b - f a - integ b a g)^2 +
        (g b - g a + integ b a f)^2
      where f = poly fargs
            g = poly gargs

    ranges = [let x = fromIntegral i / fromIntegral nDiv
              in (x*2*pi, (x+1)*2*pi)
             | i<-[0..nDiv]]

    poly :: [Double] -> Double -> Double
    poly as x = sum $ zipWith (\a p -> a * x^p) as [0..]

    fargs = (1:) $ map snd $ filter (even . fst) $ zip [0..] args
    gargs = (0:) $ map snd $ filter (odd  . fst) $ zip [0..] args

    nDiv :: Int
    nDiv = 100

    nIntegral :: Int
    nIntegral = 100

    integ :: Double ->  Double -> ( Double -> Double ) -> Double
    integ bot top f =
      let dx = (top-bot) / fromIntegral nIntegral in
        sum $
        map (\(w,x) -> w * dx * f x) $
        map (\i -> (if i==0 || i==nIntegral then 0.5 else 1,
                    dx * fromIntegral i)) $
        [0..nIntegral]
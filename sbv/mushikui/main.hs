import Control.Monad
import Data.SBV

cnt :: SWord8 -> (SWord8, SWord8) -> SWord8
cnt tgt (x10, x1) =
  (oneIf $ x10 .>0 &&& x10 .== tgt) + 
  (oneIf $ x1  .== tgt)



problem :: Symbolic SBool
problem = do
  pops <- sequence [sWord8 $ "pop" ++ show i | i <- [0..9]]
  mapM_ (constrain . (.>0))   pops
  mapM_ (constrain . (.<100)) pops
  
  let digits = map (\n -> sDivMod n 10) pops
      pops' = 
        [foldl (+) 1 $ map (cnt tgt) digits | tgt<-[0..9]]
      
  return $ bAnd $ zipWith (.==) pops pops'


main :: IO ()
main = do
  print =<< allSat problem
  
{-
Solution #1:
  pop0 = 1 :: SWord8
  pop1 = 11 :: SWord8
  pop2 = 2 :: SWord8
  pop3 = 1 :: SWord8
  pop4 = 1 :: SWord8
  pop5 = 1 :: SWord8
  pop6 = 1 :: SWord8
  pop7 = 1 :: SWord8
  pop8 = 1 :: SWord8
  pop9 = 1 :: SWord8
Solution #2:
  pop0 = 1 :: SWord8
  pop1 = 7 :: SWord8
  pop2 = 3 :: SWord8
  pop3 = 2 :: SWord8
  pop4 = 1 :: SWord8
  pop5 = 1 :: SWord8
  pop6 = 1 :: SWord8
  pop7 = 2 :: SWord8
  pop8 = 1 :: SWord8
  pop9 = 1 :: SWord8
Found 2 different solutions.
-}
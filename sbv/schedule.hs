{-# LANGUAGE ScopedTypeVariables #-}

import           Control.Monad
import           Data.SBV
import qualified Data.Vector as V
import           Data.Vector ((!))
import           Text.Printf

main = do
  str <- readFile "schedule.dat"
  let xss :: V.Vector (V.Vector Integer)
      xss = V.fromList $ map V.fromList $
            map (map read) $ map words $ lines str
      numDays = V.length xss
      numWorkers = V.length (xss!0)
  (print =<<) $ sat $ do
    let mkVar :: Int -> Int -> Symbolic (SBool)
        mkVar i j = exists $ printf "b_%d_%d" i j 
    kintais <- V.sequence $ flip V.imap xss $ \j xs ->
      V.sequence $ flip V.imap xs $ \ i _ ->
        mkVar i j
    sequence_ $ flip map [0..numDays-1] $ \j -> do
      let kintaiInt :: SInt16
          kintaiInt = fromBitsLE $ 
                      take 16 $
                      V.toList (kintais!j) ++ repeat (false::SBool)
      constrain $ sbvPopCount kintaiInt .>= 3
    return $ (true :: SBool)
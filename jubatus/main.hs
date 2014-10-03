{-# LANGUAGE OverloadedStrings #-}
import Control.Monad
import Data.List (intercalate)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Text.Printf
import System.Process
import System.Random

dim :: Int
dim = 10

score :: [Double] -> Double
score xs = sum $ map (**2) $ zipWith (-) xs [0..fromIntegral dim-1]

main :: IO ()
main = forever $ do
  cand <- replicateM dim $ randomRIO (0,fromIntegral dim :: Double)
  print cand
  tmpl <- T.readFile "train-template.rb"
  let mapStr = intercalate "," $
        ( printf "'a' => %f" (sum $ map (**2) cand)
        : printf "'c' => 1"
        : zipWith (printf "'b%02d' => %f") [(0 :: Int)..] cand)
      script2 = T.replace "MAP_HERE" (T.pack mapStr) $ 
                T.replace "SCORE_HERE" (T.pack $ show $ score cand ) tmpl
  T.writeFile "train.rb" script2
  system "ruby train.rb"
  return ()
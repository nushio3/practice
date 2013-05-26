#!/usr/bin/env runhaskell
{-# LANGUAGE TupleSections #-}

import           Control.Monad
-- import qualified Data.Vector as V
-- import           Data.Vector (Vector, (!))
import           Text.Printf

imap :: (Int -> a -> b) -> [a] -> [b]
imap f = zipWith f [0..]

type Pt = (Double, Double)

dist :: Pt -> Pt -> Double
dist (x1, y1) (x2, y2) = sqrt $ (x1-x2)^2 + (y1-y2)^2


main = do
  str <- getContents
  let p1 = lines str
      format :: Int
      format = case head $ words $ head p1 of
        "#F2" -> 2
        _     -> 1

      p2 =
        filter (\(_,(_,c)) -> c/=' ') $
        concat $
        imap (\y xas -> map (y,) xas) $
        map (imap (\x a -> (x,a))) p1

      p3 :: [(Double, Double)]
      p3 =
        map (\(x:y:_) -> (x,y)) $
        map (map read . words)$
        filter (not . isComment) p1

      isComment ('#':_) = True
      isComment _       = False

      pts :: [(Double, Double)]
      pts = case format of
                 1 -> map (\(y,(x,_)) -> (fromIntegral x, fromIntegral y)) p2
                 2 -> p3

  let dx :: Double ; xsize :: Int
      dx = 0.2
      xsize = 248*5 -- ceiling $ (/dx) $ fromIntegral $ maximum $ map length p1
  let dy :: Double ; ysize :: Int
      dy = 0.2
      ysize = 126*5 -- ceiling $ (/dy) $ fromIntegral $ length p1

      ip2p :: (Int,Int) -> Pt
      ip2p (x,y) = (fromIntegral x*dx,fromIntegral y*dy)

  let tbl :: [[Double]]
      tbl = [[ heat x y | x <- [0..xsize] ] | y <- [0..ysize]]
      heat x y = dists2heat $ map (dist $ ip2p (x,y)) pts

      dists2heat :: [Double] -> Double
      dists2heat dists
        = sum [ (1/) $ (x-14)^2+10 | x<- dists ]
--        = maximum [ sum[(1/) $ (x-y)^2+1 | x<- dists ] | y <- dists]

  let pprint :: Int -> Int -> Double -> String
      pprint x y a =
        printf "%f %f %f" (fromIntegral x*dx) (fromIntegral y*dy) a

  writeFile "points.txt" $ unlines $
    map (\(x,y) -> printf "%f %f" x y) pts

  writeFile "tbl.txt" $ unlines $
    imap (\y xas ->
           unlines $
           imap (\x a-> pprint x y a) xas) tbl
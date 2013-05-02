#!/usr/bin/env runhaskell
{-# LANGUAGE TupleSections #-}

import           Control.Monad
import qualified Data.Vector as V
import           Text.Printf
main = do
  str <- getContents
  let p1 = lines str
      imap :: (Int -> a -> b) -> [a] -> [b]
      imap f = zipWith f [0..]

      p2 =
        filter (\(_,(_,c)) -> c/=' ') $
        concat $
        imap (\y xas -> map (y,) xas) $
        map (imap (\x a -> (x,a))) p1


  forM_ p2 (\(x,(y,_)) -> printf "%d %d\n" x y)
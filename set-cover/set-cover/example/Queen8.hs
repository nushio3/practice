module Main where

import qualified Math.SetCover.Exact as ESC

import Control.Monad (liftM2)

import qualified Data.Array as Array
import qualified Data.Set as Set
import Data.Array (accumArray)
import Data.Set (Set)
import Data.List.HT (sliceVertical)
import Data.List (intersperse)
import Data.Maybe (catMaybes)


n :: Int
n = 8

range :: [Int]
range = [0 .. n-1]


data X = Row Int | Column Int | Diag Int | Gaid Int
         deriving (Eq, Ord, Show)

type Assign = ESC.Assign (Maybe (Int, Int)) (Set X)

assign :: Int -> Int -> Assign
assign i j =
   ESC.assign (Just (i,j)) $
   Set.fromList [Row i, Column j, Diag (i+j), Gaid (i-j)]

fill :: X -> Assign
fill = ESC.assign Nothing . Set.singleton

assigns :: [Assign]
assigns =
   liftM2 assign range range
   ++
   map (fill . Diag) [0 .. 2*(n-1)]
   ++
   map (fill . Gaid) [1-n .. n-1]

format :: [Maybe (Int,Int)] -> String
format =
   unlines . map (intersperse ' ') . sliceVertical n . Array.elems .
   accumArray (flip const) '.' ((0,0),(n-1,n-1)) .
   map (flip (,) 'Q') . catMaybes


main :: IO ()
main = mapM_ (putStrLn . format) $ ESC.partitions assigns

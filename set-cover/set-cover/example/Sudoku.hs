module Main where

import qualified Math.SetCover.BitSet as BitSet
import qualified Math.SetCover.Bit as Bit
import qualified Math.SetCover.Exact as ESC

import Data.Word (Word32, Word64)

import Control.Monad (liftM3, guard)

import qualified Data.Array as Array
import qualified Data.Set as Set
import Data.Array (array)
import Data.Set (Set)
import Data.List.HT (sliceVertical)
import Data.List (intersperse)


data X = Pos Int Int | Row Int Int | Column Int Int | Square Int Int Int
         deriving (Eq, Ord, Show)

type Assign = ESC.Assign ((Int, Int), Int)

assign :: Int -> Int -> Int -> Assign (Set X)
assign k i j =
   ESC.assign ((i,j), k) $
   Set.fromList [Pos i j, Row k i, Column k j, Square k (div i 3) (div j 3)]

assigns :: [Assign (Set X)]
assigns = liftM3 assign [1..9] [0..8] [0..8]


type Word81 = Bit.Sum Word64 Word32
type Mask =
   BitSet.Set
      (Bit.Sum
         (Bit.Sum Word81 Word81)
         (Bit.Sum Word81 Word81))

bit9x9 :: Int -> Int -> Word81
bit9x9 i j =
   let k = i*9+j
   in  if k<64
         then Bit.bitLeft k
         else Bit.bitRight (k-64)

bitAssign :: Int -> Int -> Int -> Assign Mask
bitAssign k i j =
   ESC.assign ((i,j), k) $
   BitSet.Set $
   Bit.Sum
      (Bit.Sum (bit9x9 k i) (bit9x9 k j))
      (Bit.Sum (bit9x9 i j) (bit9x9 k (div i 3 + 3 * div j 3)))

bitAssigns :: [Assign Mask]
bitAssigns = liftM3 bitAssign [1..9] [0..8] [0..8]


format :: [((Int, Int), Int)] -> String
format =
   unlines . map (intersperse ' ') . sliceVertical 9 . Array.elems .
   fmap (\n -> toEnum $ n + fromEnum '0') .
   array ((0,0),(8,8))


exampleHawiki1 :: [String]
exampleHawiki1 =
   "    6  8 " :
   " 2       " :
   "  1      " :
   " 7    1 2" :
   "5   3    " :
   "      4  " :
   "  42 1   " :
   "3  7  6  " :
   "       5 " :
   []

stateFromString ::
   (ESC.Set set) =>
   [Assign set] ->
   (Int -> Int -> Int -> Assign set) ->
   [String] -> ESC.State ((Int, Int), Int) set
stateFromString asgns asgn css =
   foldl (flip ESC.updateState) (ESC.initState asgns) $
   do (i,cs) <- zip [0..] css
      (j,c)  <- zip [0..] cs
      guard $ c/=' '
      return $ asgn (fromEnum c - fromEnum '0') i j


main, mainAll, mainSolve, mainBit :: IO ()
mainAll =
   mapM_ (putStrLn . format) $ ESC.partitions bitAssigns

mainSolve =
   mapM_ (putStrLn . format) $ ESC.search $
   stateFromString assigns assign exampleHawiki1

mainBit =
   mapM_ (putStrLn . format) $ ESC.search $
   stateFromString bitAssigns bitAssign exampleHawiki1

main = mainBit

{-
<https://en.wikipedia.org/wiki/Nonogram>
<https://de.wikipedia.org/wiki/Datei:Paint_by_numbers_Animation.gif>
-}
module Main where

import qualified Math.SetCover.Exact as ESC

import Control.Monad (liftM2)

import qualified Data.Set as Set
import qualified Data.List.Match as Match
import qualified Data.List.HT as ListHT
import qualified Data.List as List
import Data.Foldable (foldMap)
import Data.Char (isSpace)
import Data.Set (Set)


data X = X Orientation Int Item
        deriving (Eq, Ord, Show)

data Item = Brick Int | Position Int | Reserve Int Int
        deriving (Eq, Ord, Show)

data Orientation = Horizontal | Vertical
        deriving (Eq, Ord, Show)


type Assign = ESC.Assign (Set (Int, Int)) (Set X)

assignsFromBrick ::
   Orientation -> Int -> Int ->
   Maybe Int -> Int -> Maybe Int -> Int -> [Assign]
assignsFromBrick orient width line prevBrick thisBrick maybeThisBrick size =
   flip map [0 .. width-size] $ \col ->
   ESC.assign
      (case orient of
          Horizontal -> Set.fromList $ take size $ map ((,) line) [col ..]
          Vertical -> Set.empty) $
   Set.fromList $ map (X orient line) $
   Brick thisBrick
   :
   (map Position $ take size [col ..])
   ++
   maybe []
      (\brick -> map (Reserve brick) [col .. pred width])
      prevBrick
   ++
   maybe []
      (\brick -> map (Reserve brick) [0 .. min (pred width) (col+size)])
      maybeThisBrick

assignsFromLine ::
   Orientation -> Int -> Int -> [Int] -> [Assign]
assignsFromLine orient width line xs =
--   let bricks = Match.take (ListHT.laxTail xs) [0..]
   let bricks = Match.take (drop 1 xs) [0..]
   in  concat
          (List.zipWith4
              (assignsFromBrick orient width line)
              (Nothing : map Just bricks) [0..] (map Just bricks ++ [Nothing]) xs)
       ++
       liftM2
          (\brick c ->
             ESC.assign Set.empty $ Set.singleton $
             X orient line (Reserve brick c))
          bricks [0 .. width-1]

assignsFromLines :: [[Int]] -> [[Int]] -> [Assign]
assignsFromLines rows columns =
   concat (zipWith (assignsFromLine Horizontal (length columns)) [0..] rows)
   ++
   concat (zipWith (assignsFromLine Vertical (length rows)) [0..] columns)
   ++
   liftM2
      (\r c ->
         ESC.assign Set.empty $
         Set.fromList
            [X Horizontal r (Position c),
             X Vertical c (Position r)])
      (Match.take rows [0..])
      (Match.take columns [0..])

decode :: [[Int]] -> [[Int]] -> [Set (Int, Int)]
decode rows columns =
   map Set.unions $ ESC.partitions $ assignsFromLines rows columns

encodeLines :: [String] -> [[Int]]
encodeLines =
   map (filter (>0) . map length . ListHT.chop isSpace)

encodeStrings :: [String] -> ([[Int]], [[Int]])
encodeStrings xs =
   (encodeLines xs, encodeLines $ List.transpose xs)


testRows, testColumns :: [[Int]]
testRows =
   [1,1] :
   [1] :
   [1,1] :
   []

testColumns =
   [1,1] :
   [1] :
   [1,1] :
   []

testRhombus, testCircle, testP, testBigCircle :: [String]
testRhombus =
   "  X  " :
   " X X " :
   "X   X" :
   " X X " :
   "  X  " :
   []

testCircle =
   " XXX " :
   "XX XX" :
   "X   X" :
   "XX XX" :
   " XXX " :
   []

testP =
   "XXXX  " :
   "XXXXXX" :
   "XX  XX" :
   "XX  XX" :
   "XXXXXX" :
   "XXXX  " :
   "XX    " :
   "XX    " :
   "XX    " :
   []

-- cannot solve this one within 30 minutes
testBigCircle =
   "   XXXXX   " :
   " XXX   XXX " :
   " X       X " :
   "XX       XX" :
   "X         X" :
   "X         X" :
   "X         X" :
   "XX       XX" :
   " X       X " :
   " XXX   XXX " :
   "   XXXXX   " :
   []

soccerRows, soccerColumns :: [[Int]]
soccerRows =
   [3] :
   [5] :
   [3, 1] :
   [2, 1] :
   [3, 3, 4] :
   [2, 2, 7] :
   [6, 1, 1] :
   [4, 2, 2] :
   [1, 1] :
   [3, 1] :
   [6] :
   [2, 7] :
   [6, 3, 1] :
   [1, 2, 2, 1, 1] :
   [4, 1, 1, 3] :
   [4, 2, 2] :
   [3, 3, 1] :
   [3, 3] :
   [3] :
   [2, 1] :
   []

soccerColumns =
   [2] :
   [1, 2] :
   [2, 3] :
   [2, 3] :
   [3, 1, 1] :
   [2, 1, 1] :
   [1, 1, 1, 2, 2] :
   [1, 1, 3, 1, 3] :
   [2, 6, 4] :
   [3, 3, 9, 1] :
   [5, 3, 2] :
   [3, 1, 2, 2] :
   [2, 1, 7] :
   [3, 3, 2] :
   [2, 4] :
   [2, 1, 2] :
   [2, 2, 1] :
   [2, 2] :
   [1] :
   [1] :
   []


format :: Int -> Int -> Set (Int, Int) -> String
format rows columns set =
   unlines $
   ListHT.outerProduct
      (\r c -> if Set.member (r,c) set then 'X' else '.')
      (take rows [0..])
      (take columns [0..])

testSimple :: IO ()
testSimple = do
   let assigns = assignsFromLines testRows testColumns
   mapM_ (print . ESC.labeledSet) assigns
   putStrLn "set union:"
   print $ foldMap ESC.labeledSet assigns
   mapM_
      (putStrLn .
       format (length testRows) (length testColumns) .
       Set.unions) $
      ESC.partitions assigns

testImage :: IO ()
testImage =
   let (rows, columns) = encodeStrings testP
   in  mapM_ (putStrLn . format (length rows) (length columns)) $
       decode rows columns

-- too slow to complete
main :: IO ()
main =
   mapM_ (putStrLn . format (length soccerRows) (length soccerColumns)) $
   decode soccerRows soccerColumns

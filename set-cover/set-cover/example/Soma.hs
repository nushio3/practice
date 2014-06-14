{-
Algorithm by Helmut Podhaisky:
It is a depth-first search where in each stage we choose a position
where as few as possible bricks match. (see 'ew')
The function 'ESC.step' is a slightly more efficient version
that permanently manages the set of available bricks.
-}
module Main where

import qualified Math.SetCover.Exact as ESC
import qualified Math.SetCover.BitSet as BitSet
import qualified Math.SetCover.Bit as Bit
import Math.SetCover.Cuboid
          (PackedCoords(PackedCoords), Coords, Size, forNestedCoords,
           allPositions, allOrientations, packCoords, unpackCoords)

import Data.Word (Word8, Word32)

import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.List.Match as Match

import Control.Applicative (pure)
import Data.Foldable (foldMap)
import Data.Function.HT (nest)
import Data.List (minimumBy, intercalate)


shapes :: [[PackedCoords]]
shapes =
   map (map PackedCoords) $
   [[0,1,2,3], [0,1,3], [0,1,2,4], [0,1,4,5],
    [0,1,3,9], [0,1,3,10], [0,1,3,12]]

size :: Size
size = pure 3


newtype Brick = Brick Int deriving (Eq, Ord, Show)

type Mask = Set.Set (Either Brick PackedCoords)

type Assign = ESC.Assign (Map.Map PackedCoords Brick) Mask

transformedBrickAssign :: Brick -> [PackedCoords] -> [Assign]
transformedBrickAssign k =
   map (brickAssign k) . concatMap (allPositions size) .
   (if k == Brick 0 then \x->[x] else allOrientations) .
   map (unpackCoords size)

brickAssign :: Brick -> [Coords Int] -> Assign
brickAssign k ts =
   let xs = map (packCoords size) ts
   in  ESC.assign (Map.fromList $ map (flip (,) k) xs) $
       Set.fromList $ Left k : map Right xs

allAssigns :: [Assign]
allAssigns = concat $ zipWith transformedBrickAssign (map Brick [0 ..]) shapes

allMasks :: [Mask]
allMasks = map ESC.labeledSet allAssigns

writeMasks :: IO ()
writeMasks =
   writeFile "somaA.txt" $ show allMasks

format :: [Map.Map PackedCoords Brick] -> String
format v =
   let wuerfelx = Map.unions v
   in  forNestedCoords
          unlines (intercalate " | ") (intercalate " ")
          (\c ->
             maybe "." (\(Brick n) -> show n) $
             Map.lookup (packCoords size c) wuerfelx)
          size

printMask :: [Map.Map PackedCoords Brick] -> IO ()
printMask = putStrLn . format


-- Setcovering
omega :: Mask
omega = Set.unions allMasks

-- Erweiterungsoptionen für eine Lösung
e :: [Assign] -> [Mask] -> Either Brick PackedCoords -> [Assign]
e aktiv x o =
   filter (ESC.disjoint (Set.unions x) . ESC.labeledSet) $
   filter (Set.member o . ESC.labeledSet) $
   aktiv

-- Erweiterung der gesamten (partiellen) Lösungsmenge
ew :: [Assign] -> [[Assign]]
ew x =
   map (:x) $
   minimumBy Match.compareLength $
   map (e allAssigns $ map ESC.labeledSet x) $
   Set.toList $ Set.difference omega $ foldMap ESC.labeledSet x


type BitMask = BitSet.Set (Bit.Sum Word8 Word32)

packMask :: Mask -> BitMask
packMask =
   foldMap
      (BitSet.Set .
       either
          (\(Brick x) -> Bit.bitLeft x)
          (\(PackedCoords x) -> Bit.bitRight x))


testme :: Brick -> IO ()
testme b@(Brick n) =
   mapM_ (printMask . (:[]) . ESC.label) $ transformedBrickAssign b (shapes!!n)

main, mainBase, mainState, mainBits, testme0, testme1 :: IO ()
testme0 = testme $ Brick 0
testme1 = testme $ Brick 1

mainBase = do
   let lsg = map (map ESC.label) $ nest (length shapes) (concatMap ew) [[]]
   mapM_ printMask lsg
   print $ length lsg

mainState = do
   let lsg = ESC.partitions allAssigns
   mapM_ printMask lsg
   print $ length lsg

mainBits = do
   let lsg = ESC.partitions $ map (fmap packMask) allAssigns
   mapM_ printMask lsg
   print $ length lsg

main = mainBits

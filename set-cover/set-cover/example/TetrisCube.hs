{-
One solution:
0 0 0 1 | 0 7 1 1 | 0 4 1 5 | 4 4 1 2
7 B 0 8 | 7 7 5 5 | 7 3 6 5 | 4 3 2 2
B B B 8 | 7 A 5 2 | 3 3 6 2 | 4 6 6 2
A A B 8 | A A B 8 | 3 9 6 8 | 9 9 9 9

Another one with colors:
[34m0[m [34m0[m [34m0[m [34m1[m | [34m0[m [31m0[m [31m0[m [31m0[m | [34m0[m [31m0[m [33m2[m [33m2[m | [31m1[m [31m0[m [33m1[m [33m1[m
[33m0[m [34m2[m [34m0[m [34m1[m | [33m0[m [33m0[m [33m3[m [33m2[m | [34m3[m [33m1[m [33m1[m [33m2[m | [31m1[m [31m1[m [33m1[m [33m2[m
[33m0[m [34m2[m [33m3[m [34m1[m | [31m3[m [33m3[m [33m3[m [34m1[m | [34m3[m [34m3[m [33m3[m [31m2[m | [31m1[m [34m3[m [34m3[m [31m2[m
[33m0[m [34m2[m [34m2[m [34m2[m | [31m3[m [33m3[m [34m2[m [34m1[m | [31m3[m [31m3[m [31m3[m [31m2[m | [31m1[m [31m3[m [31m2[m [31m2[m


dist/build/tetris-cube/tetris-cube +RTS -N4 -M500m
-}
module Main where

import qualified Math.SetCover.Exact as ESC
import qualified Math.SetCover.BitSet as BitSet
import qualified Math.SetCover.Bit as Bit
import qualified Math.SetCover.Cuboid as Cuboid
import Math.SetCover.Cuboid (PackedCoords(PackedCoords), Coords, Size)

import qualified Control.Concurrent.PooledIO.Independent as Pool
-- alternative: ansi-terminal
import qualified Graphics.Ascii.Haha.Terminal as ANSI

import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.List.Match as Match

import Control.Applicative (pure)
import Data.Function (on)
import Data.Foldable (foldMap)
import Data.List (intercalate, sortBy)
import Data.Word (Word16, Word64)

import qualified System.IO as IO
import Utility (hPutStrLnImmediate)
import Text.Printf (printf)


shapes, blueShapes, yellowShapes, redShapes :: [[String]]
shapes = blueShapes ++ yellowShapes ++ redShapes

blueShapes =
   (
   "..." :
   ".  " :
   ":  " :
   [])
   :
   (
   "... " :
   "  .." :
   [])
   :
   (
   "..." :
   ":  " :
   ".  " :
   [])
   :
   (
   "':." :
   "  ." :
   [])
   :
   []


yellowShapes =
   (
   "..." :
   ":  " :
   [])
   :
   (
   ".. " :
   " :'" :
   [])
   :
   (
   "..." :
   " : " :
   [])
   :
   (
   " . " :
   ".:." :
   " ' " :
   [])
   :
   []


redShapes =
   (
   "..." :
   ".  " :
   ".  " :
   [])
   :
   (
   "...." :
   " .  " :
   [])
   :
   (
   ":." :
   ".." :
   [])
   :
   (
   ":  " :
   "..." :
   " . " :
   [])
   :
   []


propNumberOfAtoms :: Bool
propNumberOfAtoms = Cuboid.numberOf2LayerAtoms shapes == 64


size :: Size
size = pure 4


data Color = Blue | Yellow | Red deriving (Eq, Ord, Enum, Show)

type BrickId = (Color, Int)

type Mask = Set.Set (Either BrickId PackedCoords)

type Assign = ESC.Assign (Map.Map PackedCoords BrickId) Mask

transformedBrickAssign :: BrickId -> [String] -> [Assign]
transformedBrickAssign k =
   map (brickAssign k) . concatMap (Cuboid.allPositions size) .
   (if k==(Blue,0) then (:[]) else Cuboid.allOrientations) .
   Cuboid.coordsFrom2LayerString

brickAssign :: BrickId -> [Coords Int] -> Assign
brickAssign k ts =
   let xs = map (Cuboid.packCoords size) ts
   in  ESC.assign (Map.fromList $ map (flip (,) k) xs) $
       Set.fromList $ Left k : map Right xs

allAssigns :: [Assign]
allAssigns =
   let gen color =
          concat . zipWith transformedBrickAssign (map ((,) color) [0..])
   in  gen Blue blueShapes ++
       gen Yellow yellowShapes ++
       gen Red redShapes

allMasks :: [Mask]
allMasks = map ESC.labeledSet allAssigns

writeMasks :: IO ()
writeMasks =
   writeFile "tetriscube.txt" $ show allMasks

ansiColor :: ANSI.Color -> String
ansiColor c = ANSI.clr (ANSI.fg c)

formatBrickId :: BrickId -> String
formatBrickId (color, num) =
   ansiColor
      (case color of
          Red -> ANSI.Red
          Yellow -> ANSI.Yellow
          Blue -> ANSI.Blue)
   ++
   show num
   ++
   ansiColor ANSI.Reset


format :: [Map.Map PackedCoords BrickId] -> String
format v =
   let wuerfelx = Map.unions v
   in  Cuboid.forNestedCoords
          unlines (intercalate " | ") (intercalate " ")
          (\c ->
             maybe "." formatBrickId $
             Map.lookup (Cuboid.packCoords size c) wuerfelx)
          size

printMask :: [Map.Map PackedCoords BrickId] -> IO ()
printMask =
   hPutStrLnImmediate IO.stdout . format


type BitMask = BitSet.Set (Bit.Sum Word16 Word64)


packMask :: Mask -> BitMask
packMask =
   foldMap
      (BitSet.Set .
       either
          (\(color, n) -> Bit.bitLeft $ fromEnum color * 4 + n)
          (\(PackedCoords x) -> Bit.bitRight x))


testme :: BrickId -> IO ()
testme b@(color, num) =
   mapM_ (printMask . (:[]) . ESC.label) $
   transformedBrickAssign b $ (!!num) $
   case color of
      Red -> redShapes
      Blue -> blueShapes
      Yellow -> yellowShapes


main, mainState, mainBits, mainParallel, testme0, testme1 :: IO ()
testme0 = testme (Blue, 0)
testme1 = testme (Blue, 1)

mainState = do
   let lsg = ESC.partitions allAssigns
   mapM_ printMask lsg
   print $ length lsg

mainBits = do
   let lsg = ESC.partitions $ map (fmap packMask) allAssigns
   mapM_ printMask lsg
   print $ length lsg

mainParallel =
   Pool.run $ map snd $
   sortBy (flip Match.compareLength `on` fst) $
   let attempts =
          ESC.step $ ESC.initState $ map (fmap packMask) allAssigns
   in  (\f -> zipWith f [0..] attempts) $ \n attempt ->
          let refinedAttempts = concatMap ESC.step $ ESC.step attempt
          in  (refinedAttempts,
               IO.withFile (printf "tetriscube%02d.txt" (n::Int)) IO.WriteMode $ \h ->
                  mapM_ (hPutStrLnImmediate h . format) $
                  concatMap ESC.search refinedAttempts)

main = mainParallel

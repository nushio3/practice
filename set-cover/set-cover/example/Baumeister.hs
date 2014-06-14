{- |
Logika's Baumeister puzzle

<http://www.mathe-kaenguru.de/wettbewerb/baumeister/>
-}
module Main where

import qualified Math.SetCover.Exact as ESC
import qualified Math.SetCover.Cuboid as Cuboid
import Math.SetCover.Cuboid (PackedCoords, Coords, Size)

import qualified Data.Map as Map
import qualified Data.Set as Set

import Data.Foldable (forM_)
import Data.List (intercalate)
import Data.Maybe (mapMaybe)

import qualified System.IO as IO
import Utility (hPutStrLnImmediate)
import Text.Printf (printf)


shapes, flatShapes, spaceShapes :: [[String]]
shapes = flatShapes ++ spaceShapes

flatShapes =
   (
   "..." :
   [])
   :
   (
   "..." :
   ".  " :
   [])
   :
   (
   ".. " :
   " .." :
   [])
   :
   (
   ".." :
   ". " :
   [])
   :
   (
   "..." :
   " . " :
   [])
   :
   []


spaceShapes =
   (
   ".." :
   ": " :
   [])
   :
   (
   ".." :
   " :" :
   [])
   :
   (
   ".:" :
   " ." :
   [])
   :
   []


propNumberOfAtoms :: Bool
propNumberOfAtoms = Cuboid.numberOf2LayerAtoms shapes == 30


targetBase, target5Pips, targetPlus, targetA, targetE, targetS, targetO,
   targetNarrowStair, targetWideStair, targetPyramid :: [[String]]
targetBase =
   let line = replicate 5 '.'
   in  [replicate 5 line, ["", "", line, "", ""]]

-- impossible
target5Pips =
   let line = replicate 5 '.'
   in  [replicate 5 line,
        ".   ." :
        "     " :
        "  .  " :
        "     " :
        ".   ." :
        []]

targetPlus =
   let line = replicate 5 '.'
   in  [replicate 5 line,
        "     " :
        "  .  " :
        " ... " :
        "  .  " :
        "     " :
        []]

targetA =
   (
   "....." :
   ".   ." :
   "....." :
   ".   ." :
   ".   ." :
   []) :
   (
   " ... " :
   ".   ." :
   "....." :
   ".   ." :
   ".   ." :
   []) :
   []

targetE =
   replicate 3 $
   "..." :
   "." :
   ".." :
   "." :
   "..." :
   []

targetS =
   replicate 3 $
   "  ..." :
   "  . ." :
   ". .  " :
   "...  " :
   []

targetO =
   replicate 3 $
   "..." :
   ". ." :
   ". ." :
   "..." :
   []

targetNarrowStair =
   map (\n -> replicate 2 $ replicate n '.') [5,4,3,2,1]

targetWideStair =
   map (\n -> replicate 3 $ replicate n '.') [4,3,2,1]

targetPyramid =
   map (\n -> replicate n $ replicate n '.') [4,3,2,1]



newtype Brick = Brick Int deriving (Eq, Ord)

type Mask = Set.Set (Either Brick PackedCoords)

type Assign = ESC.Assign (Map.Map PackedCoords Brick) Mask

transformedBrickAssign :: Size -> Brick -> [String] -> [Assign]
transformedBrickAssign size k =
   map (brickAssign size k) . concatMap (Cuboid.allPositions size) .
   Cuboid.allOrientations . Cuboid.coordsFrom2LayerString

brickAssign :: Size -> Brick -> [Coords Int] -> Assign
brickAssign size k ts =
   let xs = map (Cuboid.packCoords size) ts
   in  ESC.assign (Map.fromList $ map (flip (,) k) xs) $
       Set.fromList $ Left k : map Right xs

allAssigns :: Size -> [Assign]
allAssigns size =
   concat $ zipWith (transformedBrickAssign size) (map Brick [0 ..]) shapes

initState ::
   Size -> [Coords Int] -> ESC.State (Map.Map PackedCoords Brick) Mask
initState size target =
   let targetSet = Set.fromList $ map (Cuboid.packCoords size) target
       keepRights =
          Set.fromList . mapMaybe (either (const Nothing) Just) . Set.toList
   in  ESC.initState $
       filter (flip Set.isSubsetOf targetSet . keepRights . ESC.labeledSet) $
       allAssigns size


format :: Size -> [Map.Map PackedCoords Brick] -> String
format size v =
   let wuerfelx = Map.unions v
   in  Cuboid.forNestedCoords
          unlines (intercalate " | ") (intercalate " ")
          (\c ->
             maybe "." (\(Brick n) -> show n) $
             Map.lookup (Cuboid.packCoords size c) wuerfelx)
          size

printMask :: Size -> [Map.Map PackedCoords Brick] -> IO ()
printMask size =
   hPutStrLnImmediate IO.stdout . format size


main, mainBase :: IO ()

mainBase =
   forM_
      [targetBase, targetPlus, targetA, targetE, targetS, targetO,
       targetNarrowStair, targetPyramid, targetWideStair] $
   \targetString -> do
      let target = Cuboid.coordsFromString targetString
          size = Cuboid.size target
          lsg = ESC.search $ initState size target
      if False
        then mapM_ (printMask size) lsg
        else printMask size $ head lsg
      printf "total number of solutions: %d\n\n" $ length lsg

main = mainBase

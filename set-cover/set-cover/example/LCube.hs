module Main where

import qualified Math.SetCover.Exact as ESC
import qualified Math.SetCover.BitSet as BitSet
import qualified Math.SetCover.Bit as Bit
import Math.SetCover.Cuboid
          (PackedCoords(PackedCoords), Coords, Size, forNestedCoords,
           allPositions, allOrientations, packCoords, unpackCoords,
           dz, normalForm)

import qualified Control.Concurrent.PooledIO.Independent as Pool
import qualified System.IO as IO
import Text.Printf (printf)
import Utility (hPutStrLnImmediate)

import qualified Data.Map as Map
import qualified Data.Set as Set

import Control.Applicative (pure)
import Data.Foldable (foldMap)
import Data.List (intercalate)
import Data.Word (Word64)


shape :: [PackedCoords]
shape = map PackedCoords [0,1,2,3,5]

size :: Size
size = pure 5


type Mask = Set.Set PackedCoords

type Assign = ESC.Assign Mask Mask

transformedBrickAssign :: [PackedCoords] -> [Assign]
transformedBrickAssign =
   map brickAssign . concatMap (allPositions size) .
   allOrientations . map (unpackCoords size)

brickAssign :: [Coords Int] -> Assign
brickAssign ts =
   let xs = Set.fromList $ map (packCoords size) ts
   in  ESC.assign xs xs

allAssigns :: [Assign]
allAssigns = transformedBrickAssign shape

allMasks :: [Mask]
allMasks = map ESC.labeledSet allAssigns

writeMasks :: IO ()
writeMasks =
   writeFile "lcube.txt" $ show allMasks


initStates :: [ESC.State Mask Mask]
initStates =
   map
      (\rotate ->
         ESC.updateState
            (brickAssign $ normalForm $
             map rotate $ map (unpackCoords size) shape) $
         ESC.initState allAssigns)
      [id, dz, dz.dz.dz]


format :: [Mask] -> String
format v =
   let wuerfelx =
          Map.unions $
          zipWith (\n -> foldMap (flip Map.singleton n)) [0..] $
          reverse v
   in  forNestedCoords
          unlines (intercalate " | ") (intercalate " ")
          (\c ->
             maybe "." (\n -> [toEnum $ n + fromEnum 'A']) $
             Map.lookup (packCoords size c) wuerfelx)
          size

printMask :: [Mask] -> IO ()
printMask = putStrLn . format


type BitMask = BitSet.Set (Bit.Sum Word64 Word64)

packMask :: Mask -> BitMask
packMask =
   foldMap
      (\(PackedCoords x) ->
         BitSet.Set $
         case divMod x 64 of
            (0, k) -> Bit.bitLeft k
            (1, k) -> Bit.bitRight k
            _ -> error "impossible position")


main, mainState, mainBits, mainParallel, testme :: IO ()
testme = mapM_ (printMask . (:[])) allMasks

mainState = do
   let lsg = concatMap ESC.search initStates
   mapM_ printMask lsg
   print $ length lsg

mainBits = do
   let lsg = concatMap ESC.search $ map (fmap packMask) initStates
   mapM_ printMask lsg
   print $ length lsg

mainParallel =
   Pool.runUnlimited $
   (\f -> zipWith f [0..] initStates) $ \n initState ->
      IO.withFile (printf "lcube%02d.txt" (n::Int)) IO.WriteMode $ \h ->
         mapM_ (hPutStrLnImmediate h . format) $
         ESC.search $ fmap packMask initState

main = mainParallel

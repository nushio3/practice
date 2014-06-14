module Math.SetCover.Cuboid where

import qualified Data.Map as Map
import qualified Data.Set as Set

import qualified Data.Traversable as Trav
import qualified Data.Foldable as Fold
import Control.Applicative (Applicative, liftA2, liftA3, pure, (<*>))
import Data.List (sort)


data Coords a = Coords a a a
   deriving (Eq, Ord, Show)

instance Functor Coords where
   fmap f (Coords x y z) = Coords (f x) (f y) (f z)

instance Applicative Coords where
   pure x = Coords x x x
   Coords fx fy fz <*> Coords x y z = Coords (fx x) (fy y) (fz z)

instance Fold.Foldable Coords where
   foldMap = Trav.foldMapDefault

instance Trav.Traversable Coords where
   traverse f (Coords x y z) = liftA3 Coords (f x) (f y) (f z)


coordsFromString :: [[String]] -> [Coords Int]
coordsFromString ss = do
   (planeN,plane) <- zip [0..] ss
   (rowN,row) <- zip [0..] plane
   (colN,c) <- zip [0..] row
   case c of
      ' '  -> []
      '.'  -> [Coords planeN rowN colN]
      _ -> error "forbidden character"

coordsFrom2LayerString :: [String] -> [Coords Int]
coordsFrom2LayerString ss = do
   (rowN,row) <- zip [0..] ss
   (colN,c) <- zip [0..] row
   fmap (Coords rowN colN) $
      case c of
         ' '  -> []
         '.'  -> [0]
         '\'' -> [1]
         ':'  -> [0, 1]
         _ -> error "forbidden character"

numberOf2LayerAtoms :: [[String]] -> Int
numberOf2LayerAtoms =
   Fold.sum .
   Map.intersectionWith (*)
      (Map.fromList [('.', 1), ('\'', 1), (':', 2)]) .
   Map.fromListWith (+) .
   map (flip (,) 1) .
   concat . concat


forNestedCoords ::
   (Enum a, Num a) =>
   ([z] -> b) ->
   ([y] -> z) ->
   ([x] -> y) ->
   (Coords a -> x) ->
   Coords a -> b
forNestedCoords fz fy fx f sz =
   case fmap (\k -> [0 .. k-1]) sz of
      Coords rx ry rz ->
         fz $ flip map rz $ \z ->
         fy $ flip map ry $ \y ->
         fx $ flip map rx $ \x ->
         f (Coords x y z)


newtype PackedCoords = PackedCoords Int
   deriving (Eq, Ord, Show)


dx, dy, dz :: Num a => Coords a -> Coords a
dx (Coords x y z) = Coords x (-z) y -- [1 0  0; 0 0 -1; 0 1 0]
dy (Coords x y z) = Coords (-z) y x -- [0 0 -1; 0 1 0; 1 0 0]
dz (Coords x y z) = Coords (-y) x z -- [0 -1 0; 1 0 0; 0 0 1]

rotations :: Num a => [Coords a -> Coords a]
rotations =
   liftA2 (.)
      [id, dx, dx.dx, dx.dx.dx]
      [id, dz, dz.dz, dz.dz.dz, dy, dy.dy.dy]


type Size = Coords Int

unpackCoords :: Size -> PackedCoords -> Coords Int
unpackCoords sz (PackedCoords n) =
   snd $ Trav.mapAccumL divMod n sz

packCoords :: Size -> Coords Int -> PackedCoords
packCoords sz =
   PackedCoords . Fold.foldr (\(k,x) s -> k*s+x) 0 . liftA2 (,) sz

normalForm :: (Ord a, Num a) => [Coords a] -> [Coords a]
normalForm ts = sort $ map (liftA2 subtract xyzm) ts
   where xyzm = foldl1 (liftA2 min) ts

{- |
Object must be in 'normalForm'.
-}
size :: [Coords Int] -> Coords Int
size = fmap succ . foldl1 (liftA2 max)

move :: Coords Int -> [Coords Int] -> [Coords Int]
move displacement = map (liftA2 (+) displacement)

allPositions :: Size -> [Coords Int] -> [[Coords Int]]
allPositions sz ts =
   map (flip move ts) $
   Trav.traverse (enumFromTo 0) $
   liftA2 (-) sz (size ts)

allOrientations :: (Num a, Ord a) => [Coords a] -> [[Coords a]]
allOrientations ts =
   Set.toList $ Set.fromList $
   map (normalForm . flip map ts) rotations

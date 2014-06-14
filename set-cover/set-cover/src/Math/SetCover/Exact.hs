module Math.SetCover.Exact where

import qualified Math.SetCover.BitMap as BitMap
import qualified Math.SetCover.BitSet as BitSet
import qualified Math.SetCover.Bit as Bit

import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.List as List
import qualified Data.List.Match as Match
import qualified Data.Foldable as Fold

import Prelude hiding (null)


class Set set where
   null :: set -> Bool
   disjoint :: set -> set -> Bool
   unions :: [set] -> set
   difference :: set -> set -> set
   minimize :: set -> [Assign label set] -> [Assign label set]

instance (Ord a) => Set (Set.Set a) where
   null = Set.null
   disjoint x y = Set.null $ Set.intersection x y
   unions = Set.unions
   difference = Set.difference
   minimize free =
      Fold.minimumBy Match.compareLength . Map.unionsWith (++) .
      (Fold.foldMap (flip Map.singleton []) free :) .
      map (\a -> Fold.foldMap (flip Map.singleton [a]) $ labeledSet a)

instance (Bit.C a) => Set (BitSet.Set a) where
   null = BitSet.null
   disjoint = BitSet.disjoint
   unions = Fold.fold
   difference = BitSet.difference
   minimize free available =
      let singleMin =
             BitSet.keepMinimum $ BitMap.minimumSet free $
             Fold.foldMap (BitMap.fromSet . labeledSet) available
      in  filter (not . BitSet.disjoint singleMin . labeledSet) available


data Assign label set =
   Assign {
      label :: label,
      labeledSet :: set
   }

assign :: label -> set -> Assign label set
assign = Assign


data State label set =
   State {
      availableSubsets :: [Assign label set],
      freeElements :: set,
      usedSubsets :: [Assign label set]
   }

instance Functor (Assign label) where
   fmap f (Assign lab set) = Assign lab (f set)

instance Functor (State label) where
   fmap f (State ab fp pb) =
      State (map (fmap f) ab) (f fp) (map (fmap f) pb)

initState :: Set set => [Assign label set] -> State label set
initState subsets =
   State {
      availableSubsets = subsets,
      freeElements = unions $ map labeledSet subsets,
      usedSubsets = []
   }

{-# INLINE updateState #-}
updateState :: Set set => Assign label set -> State label set -> State label set
updateState attempt@(Assign _ attemptedSet) s =
   State {
      availableSubsets =
         filter (disjoint attemptedSet . labeledSet) $
         availableSubsets s,
      freeElements = difference (freeElements s) attemptedSet,
      usedSubsets = attempt : usedSubsets s
   }


{-# INLINE step #-}
step :: Set set => State label set -> [State label set]
step s =
   if List.null (availableSubsets s) || null (freeElements s)
     then []
     else
        map (flip updateState s) $
        minimize (freeElements s) (availableSubsets s)

{-# INLINE search #-}
search :: Set set => State label set -> [[label]]
search s =
   if null (freeElements s)
     then [map label $ usedSubsets s]
     else step s >>= search

{-# INLINE partitions #-}
partitions :: Set set => [Assign label set] -> [[label]]
partitions = search . initState

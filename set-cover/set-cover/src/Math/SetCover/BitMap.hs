module Math.SetCover.BitMap where

import qualified Math.SetCover.BitSet as BitSet
import qualified Math.SetCover.Bit as Bit
import Math.SetCover.BitSet (Set(Set))
import Math.SetCover.Bit (xor, (.|.), (.&.))

import Data.Monoid (Monoid, mempty, mappend)


{-
Sliced representation of Map [0..bitSize-1] Integer.
-}
newtype Map bits = Map {unMap :: [bits]} deriving (Show)

instance (Bit.C bits) => Monoid (Map bits) where
   mempty = Map []
   mappend = add


fromSet :: Bit.C bits => Set bits -> Map bits
fromSet (Set x) = Map [x]

add :: Bit.C bits => Map bits -> Map bits -> Map bits
add (Map xs0) (Map ys0) =
   let go c xs [] = unMap $ inc (Set c) (Map xs)
       go c [] ys = unMap $ inc (Set c) (Map ys)
       go c (x:xs) (y:ys) =
          xor c (xor x y) :
          go (c.&.x .|. x.&.y .|. y.&.c) xs ys
   in  Map $ go Bit.empty xs0 ys0

inc :: Bit.C bits => Set bits -> Map bits -> Map bits
inc (Set xs0) (Map ys0) =
   let go c [] = if c==Bit.empty then [] else [c]
       go c (x:xs) = xor c x : go (c .&. x) xs
   in  Map $ go xs0 ys0


{-
Only elements from the base set are considered.
This way we can distinguish between non-members and members with count zero.
-}
minimumSet :: Bit.C bits => Set bits -> Map bits -> Set bits
minimumSet baseSet (Map xs) =
   foldr
      (\x mins ->
         case BitSet.difference mins $ Set x of
            newMins ->
               if BitSet.null newMins
                 then mins
                 else newMins)
      baseSet xs

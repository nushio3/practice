module Math.SetCover.Bit where

import qualified Data.Bits as Bits
import Data.Bits (Bits)
import Data.Word (Word8, Word16, Word32, Word64)
import Prelude hiding (null)


infixl 7 .&.
infixl 5 .|.

class Eq bits => C bits where
   empty :: bits
   complement, keepMinimum :: bits -> bits
   xor, (.&.), (.|.) :: bits -> bits -> bits

instance C Word8 where
   empty = 0
   complement = Bits.complement
   keepMinimum xs = xs .&. (-xs)
   xor = Bits.xor
   (.&.) = (Bits..&.)
   (.|.) = (Bits..|.)

instance C Word16 where
   empty = 0
   complement = Bits.complement
   keepMinimum xs = xs .&. (-xs)
   xor = Bits.xor
   (.&.) = (Bits..&.)
   (.|.) = (Bits..|.)

instance C Word32 where
   empty = 0
   complement = Bits.complement
   keepMinimum xs = xs .&. (-xs)
   xor = Bits.xor
   (.&.) = (Bits..&.)
   (.|.) = (Bits..|.)

instance C Word64 where
   empty = 0
   complement = Bits.complement
   keepMinimum xs = xs .&. (-xs)
   xor = Bits.xor
   (.&.) = (Bits..&.)
   (.|.) = (Bits..|.)


{-
cf. package largeword
-}
data Sum a b = Sum !a !b
   deriving (Eq, Show)

instance (C a, C b) => C (Sum a b) where
   empty = Sum empty empty
   complement (Sum l h) = Sum (complement l) (complement h)
   xor (Sum xl xh) (Sum yl yh) = Sum (xor xl yl) (xor xh yh)
   Sum xl xh .&. Sum yl yh = Sum (xl.&.yl) (xh.&.yh)
   Sum xl xh .|. Sum yl yh = Sum (xl.|.yl) (xh.|.yh)
   keepMinimum (Sum l h) =
      if l == empty
        then Sum empty (keepMinimum h)
        else Sum (keepMinimum l) empty

bitLeft :: (Bits a, C b) => Int -> Sum a b
bitLeft n = Sum (Bits.bit n) empty

bitRight :: (C a, Bits b) => Int -> Sum a b
bitRight n = Sum empty (Bits.bit n)

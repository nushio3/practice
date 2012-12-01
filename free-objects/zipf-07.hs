{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

import qualified Data.Vector as V
import           Data.Key (Zip(..))
import           Prelude hiding (zipWith)
import           Text.Printf

instance Zip V.Vector where
  zipWith = V.zipWith         


-- type level, first-in first-out list.
-- we don't export this to avoid minimum confusion with the outer world.
data a :| b = a :| b deriving (Eq, Show)
data Nil = Nil       deriving (Eq, Show)


-- | the type-class states that if you insert 
--   (v a) into vxS, the resulting type is vyS
class Insert v a vxS vyS | v a vxS -> vyS where
  insert :: v a -> vxS -> vyS

instance Insert f a Nil (f a :| Nil) where
  insert va Nil = va :| Nil

instance  (Insert f a vxS vyS) => Insert f a (vx :| vxS) (vx :| vyS) where
  insert va (vb :| vbS) = (vb :| insert va vbS)

vi1 :: V.Vector Int
vi1 = V.fromList [100..102]

vc1 :: V.Vector Char
vc1 = V.fromList ['a'..'c']

vd1 :: V.Vector Double
vd1 = V.fromList [1.1, 1.4, 1.9]

vf1 :: V.Vector (Double -> Char -> Int -> String)
vf1 = V.fromList [printf "%f %c %d", printf "%f,%c,%d", printf "%f-%c-%d"]

f_dci_s :: (Double -> Char -> Int -> String)
f_dci_s = printf "--- K %f I %c T %d A ---"

f_id_d :: (Int -> Double -> Double)
f_id_d n d = d^n

-- | perfom functional applications inside the container,
--   as many time as possible, and return the results.
class Reduce v f vxS r vyS | v f vxS -> r vyS where
  reduce :: v f -> vxS -> (v r, vyS)
 
instance (Functor v) => Reduce v f Nil f Nil where
  reduce vf Nil = (vf, Nil)

instance (Zip v, Reduce v f vxS r vyS) => Reduce v (a->f) (v a :| vxS) r vyS where
  reduce vf (va :| vxS) = reduce (zipWith ($) vf va) vxS

reduceFinal :: Reduce v f vxS r Nil => v f -> vxS -> v r
reduceFinal vf vxS = vr where (vr, Nil) = reduce vf vxS


class PType a r where
  spr :: a -> r


instance (Zip v, Reduce v f0 vaS r Nil) =>  PType (v i :| vaS) ((i -> f0)->v r) where
  spr (vi :| vaS) = (\f -> reduceFinal (fmap f vi) vaS)         

instance (Insert v b vaS vyS, PType vyS r) => PType vaS (v b->r) where
  spr vaS = (\vb -> spr (insert vb vaS))



forZN :: PType Nil r => r
forZN = spr Nil

main = do
  let args = insert vi1 $ insert vc1 $ insert vd1 Nil

  print $ args
{- 
fromList [1.1,1.4,1.9] :| (fromList "abc" :| (fromList [100,101,102] :| Nil))
-}

  print $ (reduceFinal vf1 args)
{-
fromList ["1.1 a 100","1.4,b,101","1.9-c-102"]
-}

  print $ (forZN vd1 vc1 vi1 f_dci_s)
{-
fromList ["--- K 1.1 I a T 100 A ---","--- K 1.4 I b T 101 A ---","--- K 1.9 I c T 102 A ---"]
-}

  print $ (forZN vi1 vd1 f_id_d      :: V.Vector Double)
{-
fromList [13780.612339822364,5.740260524389864e14,2.7093636335568943e28]
-}

  putStrLn $ unlines $ V.toList $ 
    forZN vd1 vc1 vi1 $ 
    \      d0  c0  i0 ->
      let msg :: String
          msg = printf "given %f %c %d. Their encode %f %f %f. Sum %f. Product %f."       
                              d0 c0 i0               d0 c1 i1      s0          p0 
          c1, i1 :: Double
          c1 = fromIntegral $ length ['A'..c0]
          i1 = fromIntegral (i0 :: Int)
          s0 = sum [d0,c1,i1] 
          p0 = product [d0,c1,i1]
      in msg

{-
given 1.1 a 100. Their encode 1.1 33.0 100.0. Sum 134.1. Product 3630.0000000000005.
given 1.4 b 101. Their encode 1.4 34.0 101.0. Sum 136.4. Product 4807.599999999999.
given 1.9 c 102. Their encode 1.9 35.0 102.0. Sum 138.9. Product 6783.0.
-}
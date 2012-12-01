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

f1 :: (Double -> Char -> Int -> String)
f1 = printf "--- K %f I %c T %d A ---"


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


instance (Insert v b vaS vyS, PType vyS r) => PType vaS (v b->r) where
  spr vaS = (\vb -> spr (insert vb vaS))

instance (Zip v, Reduce v f0 vaS r Nil) =>  PType (v i :| vaS) ((i -> f0)->v r) where
  spr (vi :| vaS) = (\f -> reduceFinal (fmap f vi) vaS)         


main = do
  let args = insert vi1 $ insert vc1 $ insert vd1 Nil
  print $ args
  print $ (reduceFinal vf1 args)
  print $ (spr Nil vd1 vc1 vi1 f1 :: V.Vector String)
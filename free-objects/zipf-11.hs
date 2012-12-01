{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

import qualified Data.Vector as V
import           Data.Key (Zip(..))
import           Prelude hiding (zipWith)
import           Text.Printf

instance Zip V.Vector where
  zipWith = V.zipWith         


-- type level, first-in first-out list, that contains only values of type (v a)
-- we don't export this to avoid minimum confusion with the outer world.
data Cons (v :: * -> *) a b = Cons a b deriving (Eq, Show)
data Nil (v :: * -> *) = Nil           deriving (Eq, Show)


-- | the type-class states that if you insert 
--   (v a) into vxS, the resulting type is vyS
class Insert v a vxS vyS | v a vxS -> vyS where
  insert :: v a -> vxS -> vyS

instance Insert v a (Nil v) (Cons v (v a) (Nil v)) where
  insert va Nil = Cons va Nil

instance  (Insert v a vxS vyS) => Insert v a (Cons v (v x) vxS) (Cons v (v x) vyS) where
  insert va (Cons vb vbS) = (Cons vb $ insert va vbS)


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
 
instance (Functor v) => Reduce v f (Nil v) f (Nil v) where
  reduce vf Nil = (vf, Nil)

instance (Zip v, Reduce v f vxS r vyS) => Reduce v (a->f) (Cons v (v a)  vxS) r vyS where
  reduce vf (Cons va vxS) = reduce (zipWith ($) vf va) vxS

reduceFinal :: Reduce v f vxS r (Nil v) => v f -> vxS -> v r
reduceFinal vf vxS = vr where (vr, Nil) = reduce vf vxS


class PType a r where
  spr :: a -> r


instance (Insert v b vaS vyS, PType vyS r) => PType vaS (v b->r) where
  spr vaS = (\vb -> spr (insert vb vaS))

instance (Zip v, Reduce v f0 vaS r (Nil v)) =>  PType (Cons v (v i)  vaS) ((i -> f0)->v r) where
  spr (Cons vi vaS) = (\f -> reduceFinal (fmap f vi) vaS)         



-- forZN :: forall v r. PType (Nil v) r => r
-- forZN = spr (Nil :: Nil v)

-- forZN :: PType (Nil V.Vector) (V.Vector Double -> r) => V.Vector Double -> r
forZN :: forall v r a.  PType (Cons v (v a) (Nil v)) r=> v a -> r
forZN vx = spr (Cons vx (Nil :: Nil v) :: Cons v (v a) (Nil v))

main = do
  let args = insert vi1 $ insert vc1 $ insert vd1 (Nil :: Nil V.Vector)

  print $ args

  print $ (reduceFinal vf1 args)

  print $ (forZN vd1 vc1 vi1 f_dci_s :: V.Vector String)


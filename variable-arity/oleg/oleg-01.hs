{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- Polyvariadic Zip
-- zipN :: v a -> v b -> .... v c -> (a -> b -> c -> r) -> v r
-- where v is a Zippable collection.

module ZipN where


zipN f = zip_n Z f

test1 = zipN [1,2,3] (\x -> (x::Int) + 1)
-- [2,3,4]

test2 = zipN [1,2,3] [10,11,12] (\x y -> ((x::Int) + y))
-- [11,13,15]

test3 = zipN [1,2,3] [10,11,12] [100,110,120] (\x y z -> ((x::Int) + y + z))
-- [111,123,135]



-- data Nat = Z | S Nat
data Z = Z
data S n = S n

prd :: S n -> n
prd = undefined

data HTrue
data HFalse

class ZipN n v r where
    zip_n :: n -> v -> r

instance (IsFunction t ~ b, r ~ ((t a) -> r1), ZipN' b n v r)
    => ZipN n v r where
    zip_n = zip_n' (undefined::b)

class ZipN' b n v r where
    zip_n' :: b -> n -> v -> r

instance (Functor v, UnCurryN n f, r ~ v r', UnCurryNR n f ~ (c -> r'))
    => ZipN' HTrue n (v c) (f->r) where
    zip_n' _ n v f = fmap (uncurryN n f) v

instance (v0 ~ v a, v1 ~ v b, ZipN (S n) (v (a,b)) r, Zip2 v)
    => ZipN' HFalse n v0 (v1->r) where
    zip_n' _ n v0 v1 = zip_n (S n) (zip2 v0 v1)


class Zip2 v where
    zip2 :: v a -> v b -> v (a,b)

instance Zip2 [] where
    zip2 = zip


-- type family IsFunction (t :: * -> *) :: Bool
type family IsFunction (t :: * -> *) :: *
type instance IsFunction ((->) a) = HTrue
type instance IsFunction [] = HFalse
-- Add more instances for Array and other container collections

-- UnCurry a function 'f' 'n' times
class UnCurryN n f where
    type UnCurryNR n f :: *
    uncurryN :: n -> f -> UnCurryNR n f


instance UnCurryN Z f where
    type UnCurryNR Z f = f
    uncurryN _ = id

instance UnCurryN n ((a, b) -> r) => UnCurryN (S n) (a->b->r) where
    type UnCurryNR (S n) (a->b->r) = UnCurryNR n ((a,b) -> r)
    uncurryN n f = uncurryN (prd n) (\ (a,b) -> f a b)


tcurry r = uncurryN (undefined::(S (S (S Z)))) (\a b c d -> (((a,b),c),d)) r
-- tcurry :: (((t2, t3), t1), t) -> (((t2, t3), t1), t)

{-# LANGUAGE FlexibleInstances,
    FunctionalDependencies,
    KindSignatures,
    NoMonomorphismRestriction,
    OverlappingInstances,
    ScopedTypeVariables,
    UndecidableInstances #-}
{- | uses a slight modification of some definitions in paczesiowa's article to
 define a less general zipWithN that takes the function as the last argument -}
module P4 where

import Data.HList
import Data.HList.FakePrelude
import Part1

-- * Two function:
hEndF ls f = uncurriedZipWithN f (hEnd ls)

t1 = hBuild [1] [2] [3] "hi" `hEndF` \a b c d -> (a+b+c,d)
-- > [(6,'h')]

-- * One function:

class IsZip1 (v :: * -> *) b
instance TypeCast b HTrue  => IsZip1 [] b
-- troublesome instance conflicts with the function
-- to be applied to the zipped lists
-- instance TypeCast b HTrue  => IsZip1 ((->) a) b
instance TypeCast b HFalse => IsZip1 a b

class IsZip v b | v -> b where isZ :: v -> b

instance IsZip1 v b => IsZip (v a) b where
    isZ v = undefined

class HBool b => IsZip' v b | v -> b where isZ' :: v -> b

-- | Based on hBuild' adding another parameter
class HBuild2' b l a r | b r-> a l, b a l -> r
 where
  hBuild2' :: b -> l -> a -> r

instance (HReverse l l', HFoldl ApplyZap [a] l' r)
      => HBuild2' br l a r
 where
  hBuild2' _ l x = uncurriedZipWithN x (hReverse l)

instance forall br a l b r.
    (IsZip b br, HBuild2' br (HCons a l) b r)
      => HBuild2' HTrue l a (b->r)
 where
  hBuild2' _ l x y = hBuild2' (isZ y) (HCons x l) y


forZ = hBuild2' hTrue hNil
{- ^

> *P4> :t forZ [1,2,3] (+)
> forZ [1,2,3] (+) :: Num x => [x -> x]
>
> *P4> forZ [1,2,3] [10] (+)
> [11]
>
> *P4> forZ [1,2,3] [10.0] (+)
> [11.0]
>
> *P4> forZ [1,2,3] "hi there" (,)
> [(1,'h'),(2,'i'),(3,' ')]
>
> *P4> forZ [1,2,3] "hi there" [Nothing] (,,)
> [(1,'h',Nothing)]

Minor advantage:

> *P4> :t \x -> forZ x "hi there" [Nothing] (,,)
> \x -> forZ x "hi there" [Nothing] (,,)
>   :: [x] -> [(x, Char, Maybe a)]

vs:

> *Main> :t \x -> forZN x vc1 vi1 f_dci_s
> \x -> forZN x vc1 vi1 f_dci_s
>   :: PType
>        (Cons v (v a) (Nil v))
>        (V.Vector Char
>         -> V.Vector Int -> (Double -> Char -> Int -> String) -> t) =>
>      v a -> t

-}

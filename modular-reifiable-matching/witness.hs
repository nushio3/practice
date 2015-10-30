{-# LANGUAGE DataKinds, GADTs, FlexibleContexts, FlexibleInstances, KindSignatures, MultiParamTypeClasses, OverlappingInstances,  TypeOperators #-}

data Elem (f :: * -> *) (fs :: [* -> *]) where
    Here  :: Elem f (f ': fs)
    There :: Elem f fs -> Elem f (g ': fs)

class Mem f fs where
    witness :: Elem f fs
instance Mem f (f ': fs) where
    witness = Here
instance (Mem f fs) => Mem f (g ': fs) where
    witness = There witness

data F1 x = F1 x
data F2 x = F2 x

w :: Elem F1 '[F1, F2, F1]
w = witness

main :: IO ()
main = print "hi"

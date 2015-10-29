class Mem f fs where
    witness :: Elem f fs
instance Mem f (f ': fs) where
    witness = Here
instance (Mem f fs) => Mem f (g ': fs) where
    witness = There witness

main :: IO ()
main = print "hi"

{-# LANGUAGE DeriveFunctor #-}

data Akari a = Waai a (Akari a) | Daisuki
     deriving (Functor)

data H oo gle = H oo oo oo oo oo gle
     deriving (Functor)

data Ha y oo = Ha y oo oo oo oo oo
     deriving (Functor)

data O b = OOOO|OOOOOO|O|OO
     deriving (Functor)

data Hom r a = Hom (r -> a)
     deriving (Functor)

data State s a = State (s -> (a, s))
     deriving (Functor)

data Just a = Nothing | Maybe (Maybe a)
     deriving (Functor)

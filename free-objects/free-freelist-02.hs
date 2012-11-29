import Control.Monad.Free


data Cons b a = Cons b a deriving (Eq, Show)
instance Functor (Cons b) where
  fmap f (Cons b a) = Cons b (f a)

type List a = Free (Cons a) ()

nil :: List a
nil = Pure ()


infixr 2 `cons`
cons :: a -> List a -> List a
cons x xs = Free (Cons x xs)

xs :: List Int
xs = 1 `cons` 2 `cons` 3 `cons` nil

ys :: List Int
ys = 5 `cons` 6 `cons` 7 `cons` 8 `cons` nil

zs :: List Int
zs = xs >> ys


main = do
  print zs
  print $ [1,2,3] >> [5,6,7,8]
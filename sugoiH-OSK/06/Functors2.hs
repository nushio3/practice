{-# LANGUAGE DeriveFunctor #-}

import Prelude hiding (Nothing)
import qualified Data.Maybe as DM
import Test.QuickCheck
import Test.QuickCheck.Function
import Test.Hspec
import Test.Hspec.QuickCheck(prop)

spec :: Spec
spec = do
  describe "Akari" $ do
    prop "satisfies the first functor law" $ (\akari' ->
      let akari :: Akari Int
          akari = akari'
      in          
        fmap id akari == akari)
    prop "satisfies the second functor law" $ (\akari' f' g' ->
      let f,g :: Int -> Int
          akari :: Akari Int
          f = apply f'
          g = apply g'
          akari = akari'
      in          
        fmap (g . f) akari == (fmap g . fmap f)  akari)

      
    

data Akari a = Waai a (Akari a) | Daisuki
  deriving (Eq, Show)

instance Functor Akari where
  fmap f Daisuki = Daisuki
  fmap f (Waai usushio akari)
    = Waai (f usushio) (fmap f akari)
     
akkariin :: [a] -> Akari a 
akkariin [] = Daisuki
akkariin (usushio : daisuki)
  = Waai usushio (akkariin daisuki)

instance Arbitrary a => Arbitrary (Akari a) where
  arbitrary = fmap akkariin $ arbitrary
    


data H oo gle = H oo oo oo oo oo gle
  deriving (Eq, Show)

instance Functor (H omura) where
  fmap watashi (H a hom ho mu ha desu)
    =  H a hom ho mu ha $ watashi desu


data Ha y oo = Ha y oo oo oo oo oo
  deriving (Eq, Show)

instance Functor (Ha skell) where
  fmap p (Ha y a i u e o)
   = Ha y (p a) (p i) (p u) (p e) (p o)

data O b = OOOO|OOOOOO|O|OO
  deriving (Eq, Show)

instance Functor O where
  fmap _      O = O
  fmap _     OO = OO
  fmap _   OOOO = OOOO
  fmap _ OOOOOO = OOOOOO



data Hom r a = Hom (r -> a)

instance Functor (Hom ra) where
  fmap mega (Hom hom)
     = Hom $ mega . hom 


data State s a = State (s -> (a, s))

instance Functor (State s) where
  fmap f (State s2as) = 
    State (\s0 -> let (a1,s1) = s2as s0 in (f a1, s1))


data Just a = Nothing | Maybe (Maybe a)
  deriving (Eq, Show)

instance Functor Just where
  fmap f Nothing = Nothing
  fmap f (Maybe x) = Maybe $ fmap f x

{-# LANGUAGE RankNTypes #-}
import Control.Monad.Cont

type Cont' m a = forall r. a -> m r

callCC' :: forall a m. MonadCont m => (Cont' m a -> m a) -> m a
callCC' f = callCC f' where
  f' :: (a -> m (EmptyMonad m)) -> m a
  f' g = f g'
    where
      g' :: a -> m b
      g' = (=<<) runEmptyMonad . g

newtype EmptyMonad m = EmptyMonad {runEmptyMonad :: forall c. m c}

main :: IO ()
main = print "hi"

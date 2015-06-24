{-# LANGUAGE RankNTypes #-}
import Control.Monad.Cont

newtype Bottom = Bottom { unleash :: forall a. a}
data USD1G = USD1G deriving Show


exmid :: Cont r (Either USD1G (USD1G -> Cont r Bottom))
exmid = callCC f
  where
     f k = return (Right (\x -> k (Left x)))

main :: IO ()
main = putStrLn "hi"

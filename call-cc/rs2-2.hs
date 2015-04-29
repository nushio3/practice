{-# LANGUAGE RankNTypes, TypeSynonymInstances,  ScopedTypeVariables, LiberalTypeSynonyms, ImpredicativeTypes #-}

import Control.Concurrent.MVar
import Unsafe.Coerce

type Stone = String

type B = IO ()
newtype Anything = Anything {unleash :: forall b. b}

type Not a = a -> B
type NN a = (a -> B) -> B

callCC :: forall a . ((a -> NN Anything) -> NN a) -> NN a
callCC f = \ c -> (f (\ x -> \ _ -> c x)) c

exmid :: NN (Either Stone (Stone -> NN Anything) )
exmid = callCC f
  where
     f k = \g -> g (Right (\x -> k (Left x)))

useTheWish :: Either Stone (Stone -> NN Anything) -> IO ()
useTheWish e = case e of
  Left s -> do
    putStrLn "got stone!"
    putStrLn s
  Right f -> do
    putStrLn "got god power in exchange of stone!"
    god <- runNN $ f "STONE"
    let n :: Int
        n = unleash (god :: Anything)

    putStrLn $ "The answer is:" ++ show n



runNN :: forall a. NN a -> IO a
runNN (f :: (a -> IO ())->IO ()) = do
  r <- newEmptyMVar
  f $ \x -> do
    putMVar r x
  takeMVar r

main :: IO ()
main = do
  print "hi"
  e <- runNN exmid
  useTheWish $ e

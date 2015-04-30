{-# LANGUAGE RankNTypes, TypeSynonymInstances,  ScopedTypeVariables, LiberalTypeSynonyms, ImpredicativeTypes #-}

import Unsafe.Coerce

type Stone = String

newtype B = B {unleash :: forall b. b}

type Not a = a -> B
type NN a = (a -> B) -> B

callCC :: forall a . ((a -> NN B) -> NN a) -> NN a
callCC f = \ c -> (f (\ x -> \ _ -> c x)) c

callCC' :: forall a . ((a -> NN B) -> NN a) -> NN a
callCC' (f:: (a-> NN B) -> NN a) =
  \ (c:: a -> B) -> (f (\ (x :: a) -> \ _ -> c x)) c


exmid :: NN (Either Stone (Stone -> NN B) )
exmid = callCC f
  where
     f k = \g -> g (Right (\x -> k (Left x)))

powerOfGod :: B
powerOfGod = undefined

useTheWish :: Either Stone (Stone -> NN B) -> IO ()
useTheWish e = case e of
  Left s -> do
    putStrLn "got stone!"
    putStrLn s
  Right f -> do
    putStrLn "got god power in exchange of stone!"
    let n :: Int
        n = unleash $ runNN $ f "STONE"

    putStrLn $ "The answer is:" ++ show n



runNN :: forall a. NN a -> a
runNN f = (unsafeCoerce :: B -> a) $ f $ \(x :: a) -> (unsafeCoerce :: a -> B) x

main :: IO ()
main = do
  print "hi"
  useTheWish $ runNN exmid

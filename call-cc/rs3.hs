{-# LANGUAGE RankNTypes, TypeSynonymInstances,  ScopedTypeVariables, LiberalTypeSynonyms, ImpredicativeTypes #-}


type Stone = String

type B = IO ()

type Not a = a -> B
type NN a = (a -> B) -> B

callCC :: forall a . ((a -> NN B) -> NN a) -> NN a
callCC f = \ c -> (f (\ x -> \ _ -> c x)) c

exmid :: NN (Either Stone (Stone -> NN B) )
exmid = callCC f
  where
     f k = \g -> g (Right (\x -> k (Left x)))

powerOfGod :: B
powerOfGod = undefined

useWish :: Either Stone (Stone -> NN B) -> B
useWish e = case e of
  Left s -> do
    putStrLn "got money"
    putStrLn s
  Right f -> do
    putStrLn "got power of god"
    f "1G USD" undefined
    putStrLn "used the power of god"
    return ()

main :: IO ()
main = exmid useWish

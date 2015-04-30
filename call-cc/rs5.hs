{-# LANGUAGE RankNTypes, TypeSynonymInstances,  ScopedTypeVariables, LiberalTypeSynonyms, ImpredicativeTypes #-}


data USD1G = USD1G deriving (Show)
type Story = String

-- newtype Cont r a = Cont { runCont :: (a -> r) -> r}

type Cont r a = (a -> r) -> r

useCont :: Cont a a -> a
useCont x = x id

newtype B = B { unleash :: forall a. a}

callCC :: ((a -> Cont r b) -> Cont r a) -> Cont r a
callCC f = \ c -> (f (\ x -> \ _ -> c x)) c


exmid :: Cont r (Either USD1G (USD1G -> Cont r B) )
exmid = callCC f
  where
     f k = \g -> g (Right (\x -> k (Left x)))

useWish :: Either USD1G (USD1G -> Cont Story B) -> Story
useWish w = case w of
  Left money -> "I got money: " ++ show money ++ "\n"
  Right demon -> unlines
    [ "I will pay demon $1G"
    , demon USD1G (\ b -> "Now I'm God\n Answer = " ++ show (unleash b :: Int))
    ]



main :: IO ()
main = do
  print "hi"
  putStrLn $ exmid useWish

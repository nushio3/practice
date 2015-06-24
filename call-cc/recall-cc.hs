{-# LANGUAGE RankNTypes #-}

newtype M a = M {runM :: forall r. (a -> r) -> r }


callCC :: forall a b . ((a -> M b) -> M a) -> M a
callCC f = M $ \ c -> runM (f (\ x -> M $ \ _ -> c x)) c



fromM :: M a -> a
fromM (M k) = k id

toM :: a -> M a
toM x = M (\k ->  k x)


main :: IO ()
main = do
  print $ fromM $ toM 1

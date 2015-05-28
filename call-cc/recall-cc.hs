{-# LANGUAGE RankNTypes #-}

newtype M a = M {unM :: forall r. (a -> r) -> r }


fromM :: M a -> a
fromM (M k) = k id

toM :: a -> M a
toM x = M (\k ->  k x)


main :: IO ()
main = do
  print 1

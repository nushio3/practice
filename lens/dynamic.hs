{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE RankNTypes #-}


import Data.Dynamic

data LC = LC{ unLC :: Num a => a -> a} deriving (Typeable)



main = do
  let x = LC (*2)
      dx = toDyn x
  print dx
  print $ (fromDynamic dx) >>= (\(LC f) -> return $ f (21::Int))
  print $ (fromDynamic dx) >>= (\(LC f) -> return $ f (7::Double))


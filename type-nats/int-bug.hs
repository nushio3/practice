{-# LANGUAGE TypeFamilies, DataKinds, TypeOperators #-}
{-# LANGUAGE PolyKinds, KindSignatures, UndecidableInstances #-}

import GHC.TypeLits 

-- | Type-level conditional
type family If (switch :: Bool) (true :: k) (false :: k) :: k where
  If True  t f = t
  If False t f = f

-- | Type-level difference of two numbers
type family Diff (x :: Nat) (y :: Nat) :: Nat where
  Diff x y = If (x <=? y) (y-x) (x-y)

main :: IO ()
main = do
  print $ fromSing (sing :: Sing (If (4 <=? 1) True False))
  print $ fromSing (sing :: Sing (If (1 <=? 4) True False))
  print $ fromSing (sing :: Sing (If (4 <=? 0) True False))
  print $ fromSing (sing :: Sing (If (0 <=? 4) True False))
  print $ fromSing (sing :: Sing (Diff 4 1))
  print $ fromSing (sing :: Sing (Diff 1 4))
  --print $ fromSing (sing :: Sing (Diff 4 0)) -- this doesn't work
  --print $ fromSing (sing :: Sing (Diff 0 4)) -- this doesn't work


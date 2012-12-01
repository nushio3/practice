{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}

import qualified Data.Vector as V

-- type level, first-in first-out list.
class FIFO v a xS yS | v a xS -> yS where
  insert :: v a -> xS -> yS

instance FIFO f a () (f a,()) where
  insert va () = (va, ())

instance  (FIFO f a xS yS) => FIFO f a (x,xS) (x,yS) where
  insert va (vb,vbS) = (vb, insert va vbS)


vi1 :: V.Vector Int
vi1 = V.fromList [100..102]

vc1 :: V.Vector Char
vc1 = V.fromList ['a'..'c']

vd1 :: V.Vector Double
vd1 = V.fromList [1.1, 1.4, 1.9]



main = print $ insert vi1 $ insert vc1 $ insert vd1 ()

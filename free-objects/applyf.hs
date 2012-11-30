{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

import Control.Applicative

class ApplyArg f z r where
  go :: f z -> r

instance (Applicative f) => ApplyArg f z ((z->a) -> f a) where
  go zs func = func <$> zs

instance (Applicative f, ApplyArg f z ((z->a) -> f a)) => ApplyArg f z (f y -> (y->z->a) -> f a) where
  go zs ys func = func <$> ys <*> zs


xs, ys :: [Int]
xs = [1..10]
ys = [1..10]



main = print $ (go xs (show) :: [String])

--               f b -> (b -> a) -> f a
--        f c -> f b -> (c -> b -> a) -> f a
-- f d -> f c -> f b -> (d ->c -> b -> a) -> f a
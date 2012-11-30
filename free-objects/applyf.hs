{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

import Control.Applicative

class ApplyType f b a r where
  go :: f b -> f (b -> a) -> r

instance (Applicative f, ApplyType f b a r) => ApplyType f c (b->a) (r) where
  go fc fcba = fcba <*> fc

main = print "hx"


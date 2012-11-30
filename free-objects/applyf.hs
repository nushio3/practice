{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

import Control.Applicative

class ApplyArg f func ret where
  go :: func -> ret



main = print "he"
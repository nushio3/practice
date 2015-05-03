{-# LANGUAGE RankNTypes #-}
to_double_neg :: forall a. a -> (forall r. (a->r)->r)
to_double_neg x = ($x)

from_double_neg :: forall a. (forall r. (a->r)->r) -> a
from_double_neg x = x id

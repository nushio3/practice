{-# LANGUAGE DataKinds #-}
data SumProd a = a | Prod a (SumProd a)

{-# LANGUAGE ScopedTypeVariables #-}

-- works on ghc 7.6.1

import Control.Monad (liftM2)
import qualified Data.Vector.Unboxed as V
import qualified Data.Vector.Unboxed.Mutable as MV
import Data.Vector.Storable.Mutable
import GHC.Prim (RealWorld)
import Control.Monad.Primitive
import Control.Monad
import qualified Data.Vector as VEC
import qualified Data.Vector.Generic.Mutable as GM
import Data.Int
import Data.Typeable.Internal
import Data.Primitive


data Row m s = Row (MV.MVector (PrimState m) s)

instance  forall m s. (Storable s, MV.Unbox s, Prim s, PrimMonad m) => Show (Row m s) where
    show (Row row) = do
      let xx :: m s
          xx = MV.read row 0
      "Done"

main :: IO ()
main = do
  xs <- MV.new 0
  print $ (Row xs :: Row IO Int)

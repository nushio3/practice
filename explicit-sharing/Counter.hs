module Counter where
import System.IO.Unsafe
import Data.IORef

{-# NOINLINE counter #-}
counter = unsafePerformIO (newIORef (0::Int))

newLabel :: () -> Int
newLabel () = unsafePerformIO $ do
  p <- readIORef counter
  writeIORef counter (p+1)
  return p       
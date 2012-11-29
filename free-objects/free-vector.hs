import Control.Monad.Free
import Data.Vector as V

xs, ys, zs :: Free Vector Int
xs = liftF $ fromList [1..10]

ys = Pure 2

zs = do
  x <- xs
  y <- ys
  z <- return $ 10 * x + y
  return $ z*z

main = do
  print zs

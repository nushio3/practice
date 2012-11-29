import Control.Monad.Free
import Data.Vector as V

ps, xs, ys, zs :: Free [] Int

ps = Pure 3

xs = liftF $ [1..10]
ys = liftF $ [0..9]

zs = do
  x <- xs
  y <- ys
  p <- ps
  z <- return $ 1000 * x + y^p
  return $ z

main = do
  print $ retract zs

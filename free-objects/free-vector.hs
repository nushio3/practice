import Control.Monad.Free
import Data.Vector as V

ps, xs, ys, zs :: Free Vector Int

ps = Pure 3

xs = liftF $ fromList [1..10]
ys = liftF $ fromList [0..9]

zs = do
  x <- xs
  y <- ys
  p <- ps
  z <- return $ 1000 * x + y^p
  return $ z

main = do
  print $ retract zs

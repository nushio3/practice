import Control.Monad.Free
import Data.Vector as V

data A a = A a deriving (Eq, Show)

xs, ys, zs :: Free A Int
xs = Pure 1
ys = Free (A (Pure 2))
zs = Free (A (Free (A (Pure 3))))

main = do
  print $ xs
  print $ ys
  print $ zs

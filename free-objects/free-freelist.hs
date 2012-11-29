import Control.Monad.Free
import Data.Vector as V

data Cons b a = Cons b a deriving (Eq, Show)

xs, ys, zs :: Free (Cons Int) String
xs = Pure "Nil"
ys = Free (Cons 1 (Pure "niru"))
zs = Free (Cons 1 (Free (Cons 2 (Free (Cons 3 (Pure "end"))))))

main = do
  print $ xs
  print $ ys
  print $ zs

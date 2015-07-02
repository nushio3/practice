import Control.Monad
import Data.Void
import Pipes
import Pipes.Prelude as P

data USD = USD1G deriving (Show)

type Contract = Either USD1G (USD1G -> Pipe () Void IO ())

contract :: Pipe Int Int IO ()
contract = do
  yield 1
  forever $ do
    x <- await
    yield (2 *x)



main :: IO ()
main = do
  runEffect $ each [] >-> c >-> (P.print)
  where
    c = c >-> contract

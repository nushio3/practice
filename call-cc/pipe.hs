import Control.Monad
import Pipes
import Pipes.Prelude as P


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

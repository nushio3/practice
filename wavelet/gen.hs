import Control.Monad
import System.Random

main :: IO ()
main = replicateM_ 256 $ do
  print =<< randomRIO (0,1::Double)
  
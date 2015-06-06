import Data.Void

import Control.Monad.Cont

getEff :: ContT Void IO a -> IO Void
getEff k = runContT (k >> return ()) ()


main :: IO ()
main = do
  print ""

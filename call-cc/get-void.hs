{-# LANGUAGE ScopedTypeVariables #-}
import Data.Void

import Control.Monad.Cont
import Control.Monad.IO.Class

getVoid :: ContT Void IO a -> IO Void
getVoid k = runContT (k >> return ()) f
  where
    f :: () -> IO Void
    f _ = runContT (k >> return ()) f

getEff :: ContT Void IO a -> IO ()
getEff k = runContT k f >> return ()
  where
    f :: a -> IO Void
    f _ = return undefined

ep12 :: ContT Void IO ()
ep12 = do
  liftIO $ putStrLn "Madoka : I am god"
  liftIO $ putStrLn "Homura : Seriously?!"


main :: IO ()
main = getEff ep12

import Control.Concurrent
import Control.Concurrent.STM
import qualified Control.Exception as E
import Control.Monad
import System.Directory
import System.Exit
import System.Posix.Signals


fnState :: FilePath
fnState = "state.txt"

main :: IO ()
main = do
  flag <- doesFileExist fnState
  initCtrVal <- case flag of
    False -> return 0
    True -> do
      con <- readFile fnState
      return $ read con
  ctr <- newTVarIO initCtrVal
  tid <- myThreadId
  installHandler keyboardSignal (Catch (myHandler ctr tid)) Nothing
  loop ctr
  
loop :: TVar Int -> IO ()
loop ctr = do
  ctr0 <- atomically $ do
    modifyTVar ctr (+1)
    readTVar ctr
  when (ctr0 `mod` 1000000 == 0) $ print ctr0
  loop ctr


myHandler :: TVar Int -> ThreadId -> IO ()
myHandler ctr tid = do
  ctr0 <- atomically $ readTVar ctr
  writeFile fnState $ show ctr0
  E.throwTo tid ExitSuccess
  
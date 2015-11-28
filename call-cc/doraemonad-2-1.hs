import Control.Monad
import Control.Monad.Cont
import Control.Monad.Reader
import Data.IORef

type M = ReaderT String (ContT () IO)

-- callCC :: ((a -> m b) -> m a) -> m a

main :: IO ()
main = prog `runReaderT` "genjitsu" `runContT` (const $ return ())

askWorld :: M ()
askWorld = do
  world <- ask
  liftIO $ putStrLn $ "I am in " ++ world


contCapture :: (MonadCont m, MonadIO m) => IORef (() -> m ()) -> m ()
contCapture ref = callCC $ \cc -> liftIO $ writeIORef ref cc >> return ()

prog :: M ()
prog = do
  askWorld
  exitRef <- liftIO $ newIORef undefined
  callCC $ \exit -> do
    liftIO $ writeIORef exitRef exit
    return ()

  local (const "yume") $ do
    askWorld
    liftIO $ putStrLn "Would you use exit?"
    userInput <- liftIO $ getLine
    when (userInput == "y") $ do
      exit <- liftIO $ readIORef exitRef
      exit ()
    return ()
  askWorld


newtype EitherT e m a = EitherT { runEitherT :: m (Either e a) }

instance Monad m => Monad (EitherT e m) where
  return a = EitherT $ return (Right a)
  m >>= k  = EitherT $ do
    a <- runEitherT m
    case a of
      Left  l -> return (Left l)
      Right r -> runEitherT (k r)
  fail = EitherT . fail


ask :: EitherT String IO String
ask = EitherT $ do
  putStrLn "would you like to continue?"
  str <- getLine
  case str of
    ('y':_) -> return $ Right str
    _ -> return $ Left $ "Aborting due to user message: " ++ str



main :: IO ()
main = do
  putStrLn "hello world"
  ret <- runEitherT $ do
    ask
    ask
    ask
  case ret of
    Left msg -> putStrLn msg
    Right msg -> putStrLn $ unwords $ replicate 3 msg

  
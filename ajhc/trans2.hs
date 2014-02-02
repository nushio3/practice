
newtype EitherIO e a = EitherIO { runEitherIO :: IO (Either e a) }

instance Monad (EitherIO e) where
  return a = EitherIO $ return (Right a)
  m >>= k  = EitherIO $ do
    a <- runEitherIO m
    case a of
      Left  l -> return (Left l)
      Right r -> runEitherIO (k r)
  fail = EitherIO . fail


ask :: EitherIO String String
ask = EitherIO $ do
  putStrLn "would you like to continue?"
  str <- getLine
  case str of
    ('y':_) -> return $ Right str
    _ -> return $ Left $ "Aborting due to user message: " ++ str



main :: IO ()
main = do
  putStrLn "hello world"
  ret <- runEitherIO $ do
    ask
    ask
    ask
  case ret of
    Left msg -> putStrLn msg
    Right msg -> putStrLn $ unwords $ replicate 3 msg

  
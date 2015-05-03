{-# LANGUAGE RankNTypes #-}
import Control.Monad.Cont
import Control.Monad.IO.Class

newtype Bottom = Bottom { unleash :: forall a. a}

type C = ContT Bottom
type CIO = C IO

data USD1G = USD1G deriving Show

say = liftIO . putStrLn

runCIO :: CIO a -> IO ()
runCIO m = runContT m (const $ return undefined) >> return ()
-- Are we sure that (undefined :: Bottom) above will never be used?

exmid :: CIO (Either USD1G (USD1G -> CIO Bottom))
exmid = callCC f
  where
     f k = return (Right (\x -> k (Left x)))

useTheWish :: Either USD1G (USD1G -> CIO Bottom) -> CIO ()
useTheWish e = case e of
  Left money -> say $ "I got money:" ++ show money
  Right method -> do
    say "I will pay devil the money."
    unobtainium <- method USD1G
    say $ "I am now omnipotent! The answer to everything is:"
      ++ show (unleash unobtainium :: Integer)

main :: IO ()
main = runCIO $ exmid >>= useTheWish

{-
> runhaskell bottom-encoding-monad.hs
I will pay devil the money.
I got money:USD1G

-}

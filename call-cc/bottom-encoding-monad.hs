{-# LANGUAGE RankNTypes #-}
import Control.Monad.Cont
import Control.Monad.Trans (lift)
import Control.Monad.Writer

newtype Bottom = Bottom { unleash :: forall a. a}

type C = ContT Bottom
type M = C (Writer String)

data USD1G = USD1G deriving Show

say x = lift $ tell $ x ++ "\n"

runM :: M a -> String
runM m = execWriter $
  runContT m (const $ return undefined) >> return ()
-- Are we sure that (undefined :: Bottom) above will never be used?

exmid :: M (Either USD1G (USD1G -> M Bottom))
exmid = callCC f
  where
     f k = return (Right (\x -> k (Left x)))

useTheWish :: Either USD1G (USD1G -> M Bottom) -> M ()
useTheWish e = case e of
  Left money -> say $ "I got money:" ++ show money
  Right method -> do
    say "I will pay devil the money."
    unobtainium <- method USD1G
    say $ "I am now omnipotent! The answer to everything is:"
      ++ show (unleash unobtainium :: Integer)

theStory :: String
theStory = runM $ exmid >>= useTheWish

main :: IO ()
main = putStrLn theStory

{-
> runhaskell bottom-encoding-monad.hs
I will pay devil the money.
I got money:USD1G

-}

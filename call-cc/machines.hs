{-# LANGUAGE RankNTypes, ImpredicativeTypes, ScopedTypeVariables #-}
import Data.Machine
import Data.Void

type M k a = Plan k String a

data USD = USD1G deriving (Show)

type Contract k = Either USD (USD -> M k Void)



callCC :: forall a k. ((a -> M k Void) -> M k a) -> M k a
callCC f = undefined
  where
    g :: (a -> M k Void)
    g = undefined

exmid ::  M k (Contract k)
exmid = callCC f
  where
    f k =
       return $ Right (\x -> k (Left x))


planA :: Contract k -> M k ()
planA e = case e of
  Left money ->
    yield $ "I got money: " ++ show money
  Right method -> do
    yield $ "I pay devil the money"
    u <- method USD1G
    yield $ "The answer to everything is :" ++ show (absurd u :: Integer)

-- | Construct the plan
helloMachine :: Monad m => SourceT m String
helloMachine = construct $ exmid >>= planA


main :: IO ()
main = do
  xs <- runT helloMachine
  print xs

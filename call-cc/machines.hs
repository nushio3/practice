{-# LANGUAGE RankNTypes, ImpredicativeTypes, ScopedTypeVariables #-}
import Data.Machine
import Data.Void
import Unsafe.Coerce

type M k a = Plan k String a
type PT k m a = PlanT k String m a

data USD = USD1G deriving (Show)

type Contract k m = Either USD (USD -> PT k m Void)

callCC :: forall a m k. ((a -> PT k m Void) -> PT k m a) -> PT k m a
callCC f = PlanT $
    \ kp ke kr kf ->
     runPlanT (f (\x -> PlanT $ \_ _ _ _ -> unsafeCoerce $kp x))
     kp ke kr kf

exmid ::  PT k m (Contract k m)
exmid = callCC f
  where
    f k =
       return $ Right (\x -> k (Left x))

planA :: Contract k m -> PT k m ()
planA e = case e of
  Left money ->
    yield $ "I got money: " ++ show money
  Right method -> do
    yield $ "I pay devil the money"
    u <- method USD1G
    yield $ "The answer to everything is :" ++ show (absurd u :: Integer)

helloMachine :: Monad m => SourceT m String
helloMachine = construct $ exmid >>= planA

main :: IO ()
main = do
  xs <- runT helloMachine
  print xs

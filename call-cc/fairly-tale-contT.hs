{-# LANGUAGE RankNTypes, TypeSynonymInstances, ScopedTypeVariables #-}

{-

Wadler. http://homepages.inf.ed.ac.uk/wadler/papers/dual/dual.pdf
Edward Yang. http://blog.ezyang.com/2013/04/a-classical-logic-fairy-tale/
JRF.   http://jrf.cocolog-nifty.com/column/2011/01/post-1.html
d.y.d. http://www.kmonos.net/wlog/61.html#_0538060508

  -}

import Control.Applicative
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Cont

data USD1G = USD1G deriving (Show)
data BecomeGod

-- exmid :: (MonadCont m) => m (Either USD1G (USD1G -> m BecomeGod))
-- exmid = callCC f
--   where
--     f k = return (Right (\x -> k (Left x)))

type IOC = ContT () IO

exmid :: IOC (Either USD1G (USD1G -> IOC BecomeGod))
exmid = callCC f
  where
    f k = return (Right (\x -> k (Left x)))


useTheWish :: Either USD1G (USD1G -> IOC BecomeGod) -> IO ()
useTheWish e = (\m -> runContT m return ) $ case e of
  Left u -> do
    liftIO $ putStrLn $ "Here I got money: " ++ show u
  Right f -> do
    liftIO $ putStrLn "Kami Ni Naru"
    (g :: BecomeGod) <- f USD1G
    liftIO $ putStrLn "Kami Korin "

main :: IO ()
main = runContT exmid useTheWish

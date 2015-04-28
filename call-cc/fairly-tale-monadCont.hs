{-# LANGUAGE RankNTypes, TypeSynonymInstances, ScopedTypeVariables #-}

{-

Wadler. http://homepages.inf.ed.ac.uk/wadler/papers/dual/dual.pdf
Edward Yang. http://blog.ezyang.com/2013/04/a-classical-logic-fairy-tale/
JRF.   http://jrf.cocolog-nifty.com/column/2011/01/post-1.html
d.y.d. http://www.kmonos.net/wlog/61.html#_0538060508

  -}

import Control.Applicative
import Control.Monad
import Control.Monad.Cont

data USD1G = USD1G deriving (Show)
data BecomeGod

-- exmid :: (MonadCont m) => m (Either USD1G (USD1G -> m BecomeGod))
-- exmid = callCC f
--   where
--     f k = return (Right (\x -> k (Left x)))

exmid :: Cont r (Either USD1G (USD1G -> Cont r BecomeGod))
exmid = callCC f
  where
    f k = return (Right (\x -> k (Left x)))



-- -- Cont (IO ()) is an instance of MonadCont.
-- -- so, if you have cont, you can do whatever you want!
-- use :: Cont (IO ()) a -> (a -> IO ()) -> IO ()
-- use k m = runCont k m
--
useTheWish :: Either USD1G (USD1G -> Cont (IO ()) BecomeGod) -> IO ()
useTheWish e = case e of
  Left u -> do
    putStrLn $ "Here I got money: " ++ show u
  Right f -> do
    putStrLn "Homura Chan Kimeta Watashi Kami Ni Naru"
    runCont (f USD1G) $ \ (g :: BecomeGod) -> do
      putStrLn "Hitorijanai----- "

main :: IO ()
main =  runCont exmid useTheWish

{-# LANGUAGE RankNTypes, TypeSynonymInstances, ScopedTypeVariables #-}

{-

Wadler. http://homepages.inf.ed.ac.uk/wadler/papers/dual/dual.pdf
Edward Yang. http://blog.ezyang.com/2013/04/a-classical-logic-fairy-tale/
JRF.   http://jrf.cocolog-nifty.com/column/2011/01/post-1.html
d.y.d. http://www.kmonos.net/wlog/61.html#_0538060508

  -}

import Control.Applicative
import Control.Monad
import Control.Monad.Writer
import Control.Monad.Cont

data USD1G = USD1G deriving (Show)
data BecomeGod

-- exmid :: (MonadCont m) => m (Either USD1G (USD1G -> m BecomeGod))
-- exmid = callCC f
--   where
--     f k = return (Right (\x -> k (Left x)))

type W = Writer String
type WC = ContT () W

exmid :: WC (Either USD1G (USD1G -> WC BecomeGod))
exmid = callCC f
  where
    f k = return (Right (\x -> k (Left x)))


useTheWish :: Either USD1G (USD1G -> WC BecomeGod) -> W ()
useTheWish e = (\m -> runContT m return ) $ case e of
  Left u -> do
    lift $ tell $ "Here I got money: " ++ show u ++ "\n"
  Right f -> do
    lift $tell "Kami Ni Naru\n"
    (g :: BecomeGod) <- f USD1G
    lift $ tell "Kami Korin\n"

story :: String
story = execWriter $ runContT exmid useTheWish

main :: IO ()
main = putStrLn story

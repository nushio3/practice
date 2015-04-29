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

-- Narrate the story
story :: MonadIO m => String -> m ()
story = liftIO . putStrLn

-- A lot of money.
data USD1G = USD1G deriving (Show)

-- A forbidden, omnipotent object from which you can create anything
newtype Anything = Anything {unleash :: forall a. a}

-- The IO monad equipped with continuation
type IOC = ContT () IO

-- The devil's contract
type Contract = Either USD1G (USD1G -> IOC Anything)

-- The IO that signs and returns the contract
exmid :: IOC Contract
exmid = do
  story "One day devil sais to you that either (L) he gives you $1G or (R) he will do anything you want if you give him $1G, but he get to choose."
  story "You sign the contract. The story begins."
  callCC f
  where
--  f k = return (Right (\x -> k (Left x)))

    f :: (Contract -> IOC Anything) -> IOC Contract
    f k = do
      story "Devil chooses (R)"
      return (Right (\x ->
                      story "Devil chooses (L)"
                      >> k (Left x)))


useTheWish :: Either USD1G (USD1G -> IOC Anything) -> IOC ()
useTheWish e = case e of
  Left u -> do
    story $ "You got money: " ++ show u
  Right f -> do
    story "You give the devil $1G to become a god"
    (g :: Anything) <- f USD1G
    story "You have become a omnipotent god"
    story $ "You know that the answer to everything is: " ++ show (unleash g :: Integer)

main :: IO ()
main = runContT (exmid >>= useTheWish) return

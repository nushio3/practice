{-# LANGUAGE ScopedTypeVariables #-}
import Data.Void

data M a = M { unM :: (a -> (Void, String)) -> (Void, String)}

instance Monad M where
  return a0 = M ($ a0)
  m >>= k   = M $ \ c -> unM m (\ x -> unM (k x) c)

tell :: String -> M ()
tell w0 = M $ \k -> let (b1,w1) = k () in (b1, w0++w1)

callCC :: ((a -> M b) -> M a) -> M a
callCC f = M $ \ c -> unM (f (\ x -> M $ \ _ -> c x)) c

exmid :: M (Either a (a -> M b))
exmid = callCC f
  where
    f k = return (Right (\x -> k (Left x)))

ep12 :: M ()
ep12 = do
  tell "Madoka : Watashi Kami Ni Naru\n"
  tell "Homura : Yamete\n"
  tell "Kyubey : QPPY!\n"
  r <- exmid
  case (r :: Either Int (Int -> M Void)) of
   Left x -> do
     tell "Madoka : I am rich: I got USD_"
     tell (show x)
   Right k -> do
     tell "Madoka : I will give kyubey my most important thing and will become god.\n"
     v <- k 100
     tell "Madoka : Now I am god! I can do anything!"
     tell (show (absurd v :: Double))
getStory :: forall a. M a -> String

getStory k = snd $ unM k f
  where
    f :: (a -> (Void, String))
    f = (\ _ -> let (v,_) = unM k f in (v,""))


interceptVoid :: M Int
interceptVoid = M $ \k -> let (v,_) = k 42 in (v,"The ansewr is:" ++ show (absurd v :: Double))

main :: IO ()
main = do
  putStrLn $ getStory ep12
  putStrLn $ getStory interceptVoid

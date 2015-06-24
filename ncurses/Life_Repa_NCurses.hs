module Main where

import Control.Monad (forM_)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.ST (runST)
import System.Random (getStdRandom,random)
import System.Environment (getArgs)
import System.Exit (exitSuccess)
import UI.NCurses
import qualified Data.Array.Repa as R
import Data.Array.Repa.Algorithms.Randomish (randomishIntArray)


type Field = R.Array R.U R.DIM2 Int

initialize :: (Int,Int) -> IO Field
initialize (x,y) = do
  seed <- getStdRandom random
  return $ randomishIntArray (R.Z R.:.x R.:.y) 0 1 seed

aggregate :: Field -> Field
aggregate f = runST $ do
  R.computeP $ R.traverse f id (\ _ sh -> count f sh)   

count :: Field -> R.DIM2 -> Int
count f (R.Z R.:.x R.:.y) = sum $ map
  (\(i,j) -> (R.!) f (R.Z R.:.i R.:.j)) $
  [(i,j)|i<-[x-1,x,x+1],j<-[y-1,y,y+1],0<=i,i<=(xmax-1),0<=j,j<=(ymax-1)]
    where (R.Z R.:.xmax R.:.ymax) = R.extent f

step :: Field -> Field
step f = runST $ do
  R.computeP $ R.zipWith survival f (aggregate f)
    where survival x y = if x == 1 
           then case y of {3->1; 4->1; _->0;}
           else case y of {3->1; _->0;}

drawField :: Field -> Update ()
drawField f = do
 let (R.Z R.:. xmax R.:. ymax) = R.extent f
 forM_ [(x,y)|x<-[0..(xmax-1)],y<-[0..(ymax-1)]] $ \(x,y) -> do
   moveCursor (fromIntegral x) (fromIntegral y)
   drawString $ case f R.! (R.Z R.:. x R.:. y) of {1->"o";_->" ";}



main :: IO ()
main = do
  [arg] <- getArgs -- 引数は画面の更新間隔(ミリ秒)
  let delaytime = read arg
  runCurses $ do
    (x,y) <- screenSize
    f <- liftIO $ initialize (fromIntegral (x-1), fromIntegral y)
    let series = iterate step f
    setEcho False
    setCursorMode CursorInvisible
    cid <- newColorID ColorGreen ColorBlack 1
    w <- defaultWindow
    updateWindow w $ setColor cid
    forM_ series $ \c -> do 
      updateWindow w $ drawField c
      render
      listenTo w delaytime 
        (\ev -> ev == EventCharacter '\n') -- Enterキー押下で終了

listenTo :: Window -> Int -> (Event -> Bool) -> Curses ()
listenTo w n p = do
  ev <- getEvent w (Just $ fromIntegral n)
  case ev of
    Nothing -> return ()
    Just ev' -> if p ev' then liftIO exitSuccess else return ()

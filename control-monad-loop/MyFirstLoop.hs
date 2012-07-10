> {-# OPTIONS -Wall #-}
> module Main where
> import Control.Monad
> import Control.Monad.IO.Class
> import Control.Monad.Trans.Loop

Hello, this is a program written in lhs mode.
First, let's do some include
and then declare some list.


> -- | This list contains all integers
> -- starting from zero. lets check for it.
> -- 
> -- >>> print $ take 5 $ xs
> -- [0,1,2,3,4]
> --
> xs :: [Int]
> xs = [0..]


> -- | This is main
> main :: IO ()
> main = do
>   foreach xs $ \i-> do
>     liftIO $ putStr $ show i
>     when (i >= 9) exit



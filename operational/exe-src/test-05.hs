{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverlappingInstances #-}

module Main where

import Control.Monad
import Control.Monad.Trans
import Control.Monad.IO.Class
import Control.Monad.Operational.Mini hiding (interpret)
import Control.Monad.Operational.Class
import Control.Monad.Trans.Operational.Mini

data SLang x where
  ReadStr :: SLang String
  WriteStr :: String -> SLang ()
  
data ILang x where
  ReadInt :: ILang Int
  WriteInt :: Int -> ILang ()
  
readStr :: Operational SLang m => m String
readStr = singleton ReadStr
  
writeStr :: Operational SLang m => String -> m ()
writeStr = singleton . WriteStr

readInt :: Operational ILang m => m Int
readInt = singleton ReadInt
  
writeInt :: Operational ILang m => Int -> m ()
writeInt = singleton . WriteInt

myProgram :: (Operational SLang m, Operational ILang m) => m ()
myProgram = do
  str <- readStr
  writeStr "Length of that str is"
  writeInt $ length str
  n <- readInt
  writeStr "you wanna have it n times; here we go:"
  writeStr $ replicate n 'H'
  

runSLang :: forall m a . MonadIO m => ProgramT SLang m a -> m a
runSLang = interpret go
  where
    go :: forall x. SLang x -> m x
    go ReadStr = liftIO getLine
    go (WriteStr str) = liftIO $ putStrLn str

runILang :: forall m a . (MonadIO m, Functor m) => ProgramT ILang m a -> m a
runILang = interpret go
  where
    go :: forall x. ILang x -> m x
    go ReadInt = fmap read $ liftIO getLine
    go (WriteInt n) = liftIO $ print n


  
main :: IO ()
main =  runILang $ runSLang $ myProgram

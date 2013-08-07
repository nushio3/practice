{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
module Main where

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Operational.Mini hiding (interpret)
import Control.Monad.Operational.Class
import Control.Monad.Trans.Operational.Mini
import Data.Char (isUpper)

-- string language, its IO interpreter, and its instructions

data SLang x where
  ReadStr :: SLang String
  WriteStr :: String -> SLang ()

slangIO :: (MonadIO m) => SLang x -> m x
slangIO ReadStr = liftIO getLine
slangIO (WriteStr str) = liftIO $ putStrLn str

readStr :: (SLang :-> f , f :! m) => m String
readStr = singletonEmbed ReadStr

writeStr :: (SLang :-> f , f :! m) => String -> m ()
writeStr = singletonEmbed . WriteStr

-- integer language, its IO interpreter, and its instructions

data ILang x where
  ReadInt :: ILang Int
  WriteInt :: Int -> ILang ()

ilangIO :: (MonadIO m) => ILang x -> m x
ilangIO ReadInt = liftIO $ liftM read getLine
ilangIO (WriteInt n) = liftIO $ print n

readInt :: (ILang :-> f , f :! m) => m Int
readInt = singletonEmbed ReadInt

writeInt :: (ILang :-> f , f :! m) => Int -> m ()
writeInt = singletonEmbed . WriteInt

-- double precision float language, its IO interpreter, and its instructions

data DLang x where
  ReadDouble :: DLang Double
  WriteDouble :: Double -> DLang ()

dlangIO :: (MonadIO m) => DLang x -> m x
dlangIO ReadDouble = liftIO $ liftM read getLine
dlangIO (WriteDouble n) = liftIO $ print n

readDouble :: (DLang :-> f , f :! m) => m Double
readDouble = singletonEmbed ReadDouble

writeDouble :: (DLang :-> f , f :! m) => Double -> m ()
writeDouble = singletonEmbed . WriteDouble



-- compose two data and create new language data.
data Either1 f g a = Left1 (f a) | Right1 (g a)

-- compose two interpreters.
(&!&) :: forall m f g a. (MonadIO m) =>
         (forall x. f x -> m x) ->
         (forall x. g x -> m x) ->
         (forall x. (Either1 f g) x -> m x)
intF &!& intG = \prog -> case prog of
  Left1  progF -> intF progF
  Right1 progG -> intG progG


-- Embed a language into a larger language.
class f :-> g where
    embed :: f a -> g a
instance f :-> f where
    embed = id

instance f :-> (Either1 f g) where
    embed = Left1
instance (f :-> fs) => f :-> (Either1 g fs) where
    embed = Right1 . embed

singletonEmbed :: (f :-> g, g :! m) => f a -> m a
singletonEmbed = singleton . embed



type MyProgram a = ProgramT (Either1 SLang (Either1 ILang DLang)) IO a

myProgram :: MyProgram ()
myProgram = do
  writeStr "input a string, an integer, and a real number"
  str <- readStr
  n <- readInt
  x <- readDouble
  writeStr    $ drop n str
  writeInt    $ length str * round x
  writeDouble $ fromIntegral (length $ filter isUpper str) / fromIntegral (length str)

main :: IO ()
main = interpret (slangIO &!& (ilangIO &!& dlangIO)) myProgram
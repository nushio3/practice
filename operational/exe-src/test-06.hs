{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}

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

data EitherF f g a = LeftF (f a) | RightF (g a)

type MyProgram a = ProgramT (EitherF SLang ILang) IO a

runMyProgram :: MyProgram a -> IO a
runMyProgram = interpret go
  where
    go :: forall x. EitherF SLang ILang x -> IO x
    go (LeftF ReadStr) = getLine
    go (LeftF (WriteStr str)) = putStrLn str

    go (RightF ReadInt) = fmap read getLine
    go (RightF (WriteInt n)) = print n

myProgram :: MyProgram ()
myProgram = do
  str <- singleton (LeftF ReadStr)
  singleton (RightF (WriteInt $ length str))

main :: IO ()
main = runMyProgram myProgram
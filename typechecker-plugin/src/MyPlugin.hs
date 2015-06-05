module MyPlugin
  ( plugin
  ) where

import Plugins

import TcEvidence
import TcPluginM
import TcPluginExtras
import TcRnTypes
import TcType

import Coercion
import BasicTypes
import DataCon
import Type
import TyCon
import TypeRep
import TysWiredIn

import FastString
import Outputable

import OccName ( occName, occNameFS, mkTcOcc )
import Module

import Data.Either



plugin :: Plugin
plugin = defaultPlugin { tcPlugin = const $ Just myPlugin }

myPlugin :: TcPlugin
myPlugin = tracePlugin "my-plugin" $
  TcPlugin { tcPluginInit  = return ()
           , tcPluginSolve = mySolver
           , tcPluginStop  = const $ return ()
           }


mySolver :: () -> [Ct] -> [Ct] -> [Ct] -> TcPluginM TcPluginResult
mySolver () givens deriveds wanteds      = do
   tcPluginIO $ putStrLn $ showSDocUnsafe $ ppr  givens
   tcPluginIO $ putStrLn $ showSDocUnsafe $ ppr  deriveds
   tcPluginIO $ putStrLn $ showSDocUnsafe $ ppr  wanteds
   undefined

{-# LANGUAGE TemplateHaskell #-}
module Pwd where

import Language.Haskell.TH
import Language.Haskell.TH.Syntax

e :: String
e = $(fmap loc_filename qLocation >>= \mod ->  return (LitE (StringL mod) ))

main = print e

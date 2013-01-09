{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
import Data.HList.Record
import Data.HList.Label5
--- import Data.HList.MakeLabels
import Data.HList.GhcSyntax
import Data.HList.FakePrelude

--- $(makeLabels ["getAge"])

x = ( (29::Double) .=. "hottie") .*. ( (29::Int) .=. "hotoke") .*.emptyRecord

instance TypeEq Int Int HTrue
instance TypeEq Double Double HTrue
instance TypeEq Int x HFalse

main = do
  putStrLn "hi"
  print emptyRecord
  print $ hLookupByLabel (28::Int) x

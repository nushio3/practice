{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module UnitTH where

import Language.Haskell.TH
import Language.Haskell.TH.Quote
import Data.Maybe

ut :: QuasiQuoter
ut = QuasiQuoter
  { quoteType = const $ return $ ConT ''Data.Maybe.Maybe
  , quoteExp = const $ return $ ConE 'Data.Maybe.Just


  }
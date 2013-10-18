{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}

module Aqlang where

import Language.Haskell.TH

 
import Language.Haskell.TH
import Language.Haskell.TH.Quote
 
doc :: QuasiQuoter
doc = QuasiQuoter { 
  quoteExp = joinE . map cvtE . parseE ,
  quotePat = error "doc is defined only for expression context" ,  
  quoteType = error "doc is defined only for expression context" ,  
  quoteDec = error "doc is defined only for expression context" 
  }

parseE :: String -> [String]
parseE = words

cvtE :: String -> ExpQ
cvtE ('`':nam) = varE $ mkName nam
cvtE x         = stringE x

joinE :: [ExpQ] -> ExpQ
joinE = foldl ap [e| "" |] 
  where
    ap a b = appE (appE (varE '(++) ) a ) b

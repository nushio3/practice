{-# LANGUAGE TemplateHaskell #-}
module EmbedExpr where

import Language.Haskell.TH
import Language.Haskell.TH.Quote
import Language.Haskell.Meta.Parse (parseExp)

infixr 2 :.

myQ :: QuasiQuoter
myQ = QuasiQuoter
  { quoteExp = embed . parse }

-- intermediate representation
data IR 
  = IR :. IR
  | StringPart String
  | EmbedPart String
  deriving (Eq, Show)
    
-- a loose parser to IR           
parse :: String -> IR    
parse str = parseS str
  where
    parseS x = case break (=='{') x of
      (s,('{':e)) -> (:.) (StringPart s) $ parseE e
      (s,_) -> StringPart s
    parseE x = case break (=='}') x of
      (e,('}':s)) -> (:.) (EmbedPart e) $ parseS s
      (e,_) -> EmbedPart e
      
embed :: IR -> ExpQ
embed (x :. y) = (varE '(++)) `appE` (embed x) `appE` (embed y)
embed (StringPart str) = stringE str
embed (EmbedPart str) = (varE 'show) `appE` (return gotExp)
  where
    gotExp = either undefined id $ parseExp str 
      
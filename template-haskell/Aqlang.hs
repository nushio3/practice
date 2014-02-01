{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}

module Aqlang where

import Control.Applicative
import Data.Char
import Data.Monoid
import Language.Haskell.TH
import Language.Haskell.TH.Quote
import Text.Trifecta
import Text.Trifecta.Delta
import Text.Parser.LookAhead
import Text.PrettyPrint.ANSI.Leijen as Pretty hiding (line, (<>), (<$>), empty, string)
import System.IO

data Component 
  = StrPart    String
  | EmbedMonad String
  | EmbedShow  String deriving (Eq,Show)

parseLang :: Parser [Component]
parseLang = (many $ choice [try embedMonad, try embedShow, strPart]) <* eof

strPart :: Parser Component
strPart = StrPart <$> go <?> "String Part"
  where
    go = do
      notFollowedBy $  choice [string "#{", string "@{"]
      h <- anyChar
      t <- manyTill anyChar (lookAhead $ choice [string "#{", string "@{", eof >> return ""])
      return $ h:t

embedMonad :: Parser Component
embedMonad = EmbedMonad <$> between (string "@{") (string "}") (some $ noneOf "}")
          <?> "Monad Part"


embedShow :: Parser Component
embedShow = EmbedShow <$> between (string "#{") (string "}") (some $ noneOf "}")
          <?> "Show Part"

 


rawQ :: QuasiQuoter
rawQ = QuasiQuoter { 
  quoteExp = parseE ,
  quotePat = error "doc is defined only for expression context" ,  
  quoteType = error "doc is defined only for expression context" ,  
  quoteDec = error "doc is defined only for expression context" 
  }

parseE :: String -> ExpQ
parseE str = do
  let res = parseString parseLang (Columns 0 0) str
  case res of
    Failure xs -> do 
      runIO $ displayIO stdout $ renderPretty 0.8 80 $ xs <> linebreak
      error "cannot parse ."
    Success x -> joinE $ map cvtE x
      

cvtE :: Component -> ExpQ
cvtE (StrPart x) = appE (varE 'putStr) (stringE x)
cvtE (EmbedShow x) = appE (varE 'print) (varE $ mkName $ trim x)
cvtE (EmbedMonad x) = (varE $ mkName $ trim x)                           

trim :: String -> String
trim = dropWhile isSpace . reverse . dropWhile isSpace . reverse


joinE :: [ExpQ] -> ExpQ
joinE = foldl ap [e| return () |] 
  where
    ap a b = appE (appE (varE '(>>) ) a ) b

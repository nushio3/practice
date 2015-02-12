module Tokenizer where

import Control.Applicative
import Text.Trifecta
import Text.PrettyPrint.ANSI.Leijen as Pretty hiding (line, (<>), (<$>), empty)
import System.Environment

varHeadChar :: Parser Char
varHeadChar = alphaNum

varTailChar :: Parser Char
varTailChar = alphaNum <|> oneOf "_"

varName :: Parser String
varName = do
  h <- varHeadChar
  t <- many varTailChar
  spaces
  return (h:t)

reserved :: String -> Parser String
reserved str = symbol str <* spaces



type Program = [Statement]

program :: Parser Program
program = (:) <$> statement <*> (program <|> pure [])

type Statement = (String, String)

statement :: Parser Statement
statement = do
  lhs <- varName
  reserved "="
  pos <- position
  rhs <- varName
  many $ reserved ";"
  return (lhs,rhs ++ "@" ++ show pos)

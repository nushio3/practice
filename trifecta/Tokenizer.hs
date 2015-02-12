module Tokenizer where

import Control.Applicative
import Text.Trifecta
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
  rhs <- varName
  many $ reserved ";"
  return (lhs,rhs)

main = do
    (fileName:_ ) <- getArgs
    parseTest (program <* eof) =<< readFile fileName

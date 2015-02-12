module Tokenizer where

import Control.Applicative
import Control.Lens
import Data.Monoid
import qualified Data.Set as S
import System.Environment
import System.IO
import Text.Trifecta
import Text.Printf
import Text.PrettyPrint.ANSI.Leijen as Pretty hiding (line, (<>), (<$>), empty, integer)


data Statement = Statement
  { lhs :: String, rhs :: Integer, renderStatement :: Rendering}

instance Show Statement where
  show (Statement a b _) = printf "%s=%d" a b

varName :: Parser String
varName = many $ oneOf ['a'..'z']


statement :: Parser Statement
statement = do
  r <- rend
  var <- token $ varName
  token $ symbol "="
  val <- token $ integer
  return $ Statement var val r


main :: IO ()
type Program = [Statement]

program :: Parser Program
program = (:) <$> statement <*> (program <|> pure [])


main = do
  (fileName:_ ) <- getArgs
  res <- parseFromFileEx (program <* eof) fileName
  case typeCheck res of
    Success prog -> mapM_ print prog
    Failure xs -> displayIO stdout $ renderPretty 0.8 80 $ xs <> linebreak



typeCheck :: Result Program -> Result Program
typeCheck x@(Failure _) = x
typeCheck (Success prog0) = go [] prog0
  where
    go _ [] = Success prog0
    go usedList (s:ss)
       | not $ (lhs s) `elem` usedList = go (lhs s:usedList) ss
       | otherwise = let r = renderStatement s in
           Failure $
                     explain (addCaret (r ^. renderingDelta) r) $
                     Err (Just $ Pretty.text "duplicate definition") [] (S.singleton "unused variable name")

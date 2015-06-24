import Control.Applicative
import Control.Lens
import Control.Monad
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
varName = (\x -> try x <?> "varName")  $ do
  [x] <- many $ oneOf ['a'..'y']
  return [x]


longVarName :: Parser String
longVarName = (\x -> try x <?> "longVarName") $ do
  [x,y] <- many $ oneOf ['a'..'y']
  return [x,y]

smallInteger :: Parser Integer
smallInteger = (\x -> try x <?> "integer smaller than 100") $ do
  x <- integer
  guard $ x < 100
  return x

statement :: Parser Statement
statement = (\x ->  x <?> "statement") $ do
  r <- rend
  var <- token $ longVarName <|> varName
  token $ symbol "="
  val <- token $ smallInteger
  return $ Statement var val r



type Program = [Statement]

program :: Parser Program
program = (:) <$> statement <*> (program <|> pure [])

main :: IO ()
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

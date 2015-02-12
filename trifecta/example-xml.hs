import Text.Trifecta
import Control.Applicative
import Text.Printf
import System.Environment

data XML = XML{
    tag :: String,
    content :: String,
    attribute :: [(String,String)],
    child :: Maybe XML
    }
instance Show XML where
  show (XML t c a Nothing)  = printf "<%s %s>%s</%s>" t (show a) c t
  show (XML t c a (Just x)) = printf "<%s %s>%s%s</%s>" t (show a) c (show x) t

runXML :: Parser [XML]
runXML = (:) <$> xml <*> (runXML <|> return [])

xml :: Parser XML
xml = do
    (tag,attrs) <- (,) <$> (symbol "<" *> some alphaNum) <*> attributes <?> "opening tag"
    (mkXML tag attrs) <$> value <*> node tag

attributes :: Parser [(String,String)]
attributes = do
    spaces
    [] <$ symbol ">" <|> (:) <$> elem <*> attributes where
        elem = (,) <$> attr <*> attrValue
        attr = many alphaNum
        term = spaces *> symbol "=" <* spaces
        attrValue = term *> stringLiteral <* spaces

value :: Parser String
value = many (noneOf "<")

node :: String -> Parser (Maybe XML)
node tagName = Nothing <$ isEnd <|> Just <$> xml <* isEnd where
    isEnd = (symbol $ "</" ++ tagName ++ ">")  <?> printf "closing tag <%s>" tagName


mkXML :: String -> [(String,String)] -> String -> Maybe XML -> XML
mkXML tag attr content child =
    XML {tag = tag, content = content,
         attribute = attr, child = child}

main = do
    (fileName:_ ) <- getArgs
    parseTest (runXML <* eof) =<< readFile fileName

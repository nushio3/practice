{-# LANGUAGE OverloadedStrings #-}

import Control.Monad
import Data.Yaml
import Data.Maybe
import Data.Monoid
import Data.OrgMode.Parse.Attoparsec.Document
import Data.OrgMode.Parse.Attoparsec.Time
import Data.OrgMode.Parse.Types
import Data.Attoparsec.Text
import qualified Data.Text as T
import qualified Data.Text.IO as T
import System.Environment

thow :: Show a => a -> T.Text
thow = T.pack . show

main :: IO ()
main = getArgs >>= mapM_ process

process :: FilePath -> IO ()
process fn = do
  input <- T.readFile fn
  let r = parse (parseDocument []) input
  case r of
    Done remain doc -> do
                       use doc
                       putStrLn "DISCONTINUED!"
                       T.putStrLn $ T.take 100 remain
    Partial k -> case k "" of
      Done _ doc -> use doc
      x -> print x
    x -> print x

use :: Document -> IO ()
use doc = do
  mapM_ printHeading $ documentHeadings doc
  encodeFile "test.yaml" doc
  T.writeFile "test.txt" $ thow doc

printHeading :: Heading -> IO ()
printHeading h = do
  let t = sum $ map durationMin $ (ts1 ++ ts2)
      ts1 = sectionClocks $ section h
      ts2 = map p1 $
            T.lines $ sectionParagraph $ section h
      p1 = fromMaybe (Nothing, Nothing) . maybeResult . parse parseClock . (<> "\n")

  do
    T.putStrLn $ T.unwords [thow (level h), title h]
    print $ t
  mapM_ printHeading $ subHeadings h


durationMin :: (Maybe Timestamp, Maybe Duration) -> Int
durationMin (_, Nothing) = 0
durationMin (_, Just (h,m)) = h*60+m

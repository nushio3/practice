{-# LANGUAGE OverloadedStrings #-}

import Control.Applicative
import Control.Monad
import qualified Data.HashMap.Strict as HM
import Data.Maybe
import Data.Monoid
import Data.Yaml
import Data.OrgMode.Parse.Attoparsec.Document
import Data.OrgMode.Parse.Attoparsec.Time
import Data.OrgMode.Parse.Attoparsec.Headings
import Data.OrgMode.Parse.Types
import Data.Attoparsec.Text
import qualified Data.Text as T
import qualified Data.Text.IO as T
import System.Environment
import Text.Printf
import Text.Read (readMaybe)

data Project = Project
  { projHeading :: Heading
  , allocation :: Maybe (Double, Timestamp)
  , timeUsed :: Double
  , timeDeserve :: Double
  }


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
  let p = toProject h
  putStrLn $ printf "%5d %3.0f %s" (countHeadingTime h) (fromMaybe 0 (fst <$> allocation p))  (T.unpack $ title h)


toProject :: Heading -> Project
toProject h = Project{projHeading = h, allocation = alloc, timeUsed = 0, timeDeserve = 0}
  where
    Plns plmap = sectionPlannings $ section h

    alloc :: Maybe (Double, Timestamp)
    alloc = do
      sts <- HM.lookup SCHEDULED plmap
      guard $ HM.lookup CLOSED plmap == Nothing
      (w:_) <- return $ catMaybes $ map parseWeight $ T.lines $ sectionParagraph $ section h
      return (w,sts)

parseWeight :: T.Text -> Maybe Double
parseWeight str = do
  [k,v] <- return $ T.words str
  guard $ k == "WEIGHT:"
  readMaybe $ T.unpack v


countHeadingTime :: Heading -> Int
countHeadingTime h = t + t2
  where
      t = sum $ map durationMin $ (ts1 ++ ts2)
      ts1 = sectionClocks $ section h
      ts2 = map p1 $
            T.lines $ sectionParagraph $ section h
      p1 = fromMaybe (Nothing, Nothing) . maybeResult . parse parseClock . (<> "\n")

      t2 = sum $ map countHeadingTime $ subHeadings h

durationMin :: (Maybe Timestamp, Maybe Duration) -> Int
durationMin (_, Nothing) = 0
durationMin (_, Just (h,m)) = h*60+m

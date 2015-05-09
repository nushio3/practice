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

type Timespan      = (Timestamp, Duration)

durationOfTimespan :: Timespan -> Double
durationOfTimespan (_,(h,m)) = fromIntegral $ 60*h+m

data Project = Project
  { projHeading :: Heading
  , allocation :: Maybe (Double, Timestamp)
  , timeUsed :: Double
  , timeDeserve :: Double
  }

addProject :: Project -> Project -> Project
addProject a b = Project ((projHeading a){title="Total"}) (addA (allocation a) (allocation b)) (timeUsed a + timeUsed b) (timeDeserve a + timeDeserve b)
  where
    addA (Just (wa,ta)) (Just (wb,tb)) = Just (wa+wb, ta) -- smaller of?
    addA a b = a <|> b


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
  encodeFile "test.yaml" doc
  T.writeFile "test.txt" $ thow doc
  let projs = map toProject $ documentHeadings doc
      spans :: [Timespan]
      spans = concat $ map spansOfProj projs
      projs1 = map fillTimeUsed projs
      projs2 = foldr redistribute projs1 spans
  printf "Total Deserv   Used WGT Task\n"
  mapM_ pprProj $ projs2 ++ [foldr1 addProject projs2  ]

pprProj :: Project -> IO ()
pprProj proj = do
  let h = projHeading proj
  putStrLn $ printf "%5d %6.1f %6.1f %3.0f %s" (countHeadingTime h)
    (timeDeserve proj)
    (timeUsed proj)
    (fromMaybe 0 (fst <$> allocation proj))  (T.unpack $ title h)

fillTimeUsed :: Project -> Project
fillTimeUsed proj = foldr earnUsed proj spans
  where spans = spansOfProj proj


redistribute :: Timespan -> [Project] -> [Project]
redistribute ts projs =
  if sumShare == 0 then projs
  else map (earnDeserve ts (durationOfTimespan ts / sumShare)) projs
  where
    sumShare = sum $ map (claimShare ts) projs

claimShare :: Timespan -> Project -> Double
claimShare (ts1,_) proj = case allocation proj of
  Nothing -> 0
  Just (w, ts2) -> if show ts1 >= show ts2 then w else 0

earnDeserve :: Timespan -> Double -> Project -> Project
earnDeserve (ts1,_) share proj = case allocation proj of
  Nothing -> proj
  Just (w, ts2) -> if show ts1 >= show ts2 then proj{timeDeserve = timeDeserve proj + share*w } else proj

earnUsed :: Timespan -> Project -> Project
earnUsed t@(ts1,_) proj = case allocation proj of
  Nothing -> proj
  Just (w, ts2) -> if show ts1 >= show ts2 then proj{timeUsed = timeUsed proj + durationOfTimespan t} else proj




-- printHeading :: Heading -> IO ()
-- printHeading h = do
--   let p = toProject h
--   putStrLn $ printf "%5d %3.0f %s" (countHeadingTime h) (fromMaybe 0 (fst <$> allocation p))  (T.unpack $ title h)


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

findSpans :: Heading -> [Timespan]
findSpans h = t ++ t2
  where
      t =  concat $ map toTimespan $ (ts1 ++ ts2)
      ts1 = sectionClocks $ section h
      ts2 = map p1 $
            T.lines $ sectionParagraph $ section h
      p1 = fromMaybe (Nothing, Nothing) . maybeResult . parse parseClock . (<> "\n")

      t2 = concat $ map findSpans $ subHeadings h

      toTimespan (Just a, Just b) = [(a,b)]
      toTimespan _ = []

spansOfProj :: Project -> [Timespan]
spansOfProj proj = case allocation proj of
  Nothing -> []
  Just _  -> findSpans $ projHeading proj

durationMin :: (Maybe Timestamp, Maybe Duration) -> Int
durationMin (_, Nothing) = 0
durationMin (_, Just (h,m)) = h*60+m

{-# LANGUAGE GeneralizedNewtypeDeriving, OverloadedStrings #-}

module Main where

import Control.Applicative
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Either
import Data.Maybe (catMaybes)
import Network.Curl (curlGetString, CurlOption(..))
import Text.PrettyPrint.ANSI.Leijen(putDoc)
import Text.HTML.TagSoup
import qualified Data.Text as T



newtype NAM a = NAM {runNAM :: EitherT String IO a}
              deriving (Functor, Applicative, Monad, MonadIO)

getURL :: String -> NAM ()
getURL url = do
  (code, resp) <- liftIO $ curlGetString url [CurlFollowLocation True]
  case resp of
    "" -> NAM $ left $ show code
    _  -> do
      let tags :: [Tag T.Text]
          tags = parseTags $ T.pack resp
      liftIO $ mapM_ print $ imgURLs $ map head $ sections (~== ("<img>"::String)) tags
      liftIO $ mapM_ print $ webURLs $ map head $ sections (~== ("<a>"::String)) tags

  where
    imgURLs :: [Tag T.Text] -> [] T.Text
    imgURLs tags = do
      TagOpen _ attrs <- tags
      ("src", imgURL) <- attrs
      return imgURL

    webURLs :: [Tag T.Text] -> [] T.Text
    webURLs tags = do
      TagOpen _ attrs <- tags
      ("href", url) <- attrs
      return url

catScore :: T.Text -> Int
catScore txt = sum [factor * T.count kwd txt | (kwd, factor) <- keywords]
  where
    keywords = [("猫", 10),
                ("子猫", 100),
                ("かわいい", 50),
                ("cat", 10),
                ("kitten", 28)        ]


main :: IO ()
main = do
  ret <- runEitherT $ runNAM $ getURL "http://matome.naver.jp/topic/1M2IY"
  print ret

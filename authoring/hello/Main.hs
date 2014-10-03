{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
import qualified Data.Text as T
import qualified Data.Text.IO as T
import System.Process
import Text.Authoring
import Text.Authoring.TH
import qualified Text.LaTeX as LTX


paperBody :: MonadAuthoring s w m => m ()
paperBody = [inputQ|main-template.tex|]

main :: IO ()
main = do
  (bibText, _ , bodyTex) <- runAuthoringT $ do
    withDatabaseFile "citation.db" $ do
      paperBody
      bibliographyContent
  
  T.writeFile "main.tex" $ LTX.render bodyTex
  T.writeFile "references.bib" bibText
  system "lualatex main.tex"
  system "bibtex main"
  system "lualatex main.tex"
  system "lualatex main.tex"
  return ()
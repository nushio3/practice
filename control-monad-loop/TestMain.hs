module Main where

import Test.Framework.TH.Prime
import Test.Framework.Providers.DocTest

main :: IO ()
main = $(defaultMainGenerator)


doc_test :: DocTests
doc_test = docTest ["MyFirstLoop.hs"] ["-i.."]
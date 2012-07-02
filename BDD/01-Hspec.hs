#!/usr/bin/env runhaskell
import Test.Hspec.Monadic
import Test.Hspec.QuickCheck
import Test.HUnit
main :: IO ()
main = hspec spec >> return ()

spec :: Spec
spec = do
  describe "reverse" $ do
    it "reverses a list" $
      assertEqual "" [3, 2, 1] (reverse [1, 2, 3])
    it "gives the original list, if applied twice" $
      property $ \xs ->
      (reverse . reverse) xs == (xs :: [Int])

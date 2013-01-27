import Test.Hspec

import Asm

main = hspec spec

spec :: Spec
spec = do
  describe "Asm" $ do
    it "is stable." $ do
       (1+1) `shouldBe` 2
       
       


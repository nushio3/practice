import qualified Data.SBV as SBV
import Test.Hspec

import Asm

main = hspec spec

shouldProve :: SBV.Predicate -> Expectation
shouldProve theorem = do
  res <- SBV.prove theorem            
  show res `shouldBe` "Q.E.D."

allEqual xs ys = SBV.bAnd $ zipWith (SBV..==) xs ys

spec :: Spec
spec = do
  describe "Nop" $ do
    it "does nothing." $ do
      shouldProve $ do
        rs0 <- symbolicRegs          
        rs1 <- exec (toHex Nop) rs0
        return $ allEqual rs0 rs1
  describe "Ial" $ do
    it "sets the last 4 bits of eax." $ do
      shouldProve $ do
        rs0 <- symbolicRegs          
        rs1 <- exec (toHex $ Ial 0) rs0
        return $ allEqual rs0 rs1
    
       
       


import           Data.Bits
import qualified Data.SBV as SBV
import           Data.SBV ((.==), (.<), (.<=), (&&&))
import           Test.Hspec

import Asm

main = hspec spec

shouldProve :: SBV.Predicate -> Expectation
shouldProve theorem = do
  res <- SBV.prove theorem            
  show res `shouldBe` "Q.E.D."

allEqual xs ys = SBV.bAnd $ zipWith (.==) xs ys

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
        n <- SBV.sWord8 "n"   
        SBV.constrain $ 0 .<= n &&& n .< 16
        rs1 <- exec (toHex $ Ial n) rs0
        let (rs0h: rs0t) = rs0
            rs0' = ((rs0h .&. 0xf0) .|. n) : rs0t
        return $ allEqual rs0' rs1
    
       
       


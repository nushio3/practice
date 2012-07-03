import Fib

newtype Small = Small Int
instance Arbitrary Small where
  arbitrary = Small . (‘mod‘ 10) <$> arbitrary

main = hspec spec
spec = do
  describe "fib" $ do
    it "calculates arbitrary Fibonacci numbers" $ do
      property $ \(Small n) ->
        fib n == fib (n + 2) - fib (n + 1)

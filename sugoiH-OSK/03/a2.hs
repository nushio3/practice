import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck
import Test.QuickCheck.Function as Fun


takeWhile' :: (a -> Bool) -> [a] -> [a]
takeWhile' f [] = []
takeWhile' f (x:xs) 
  | f x       = x : takeWhile' f xs
  | otherwise = []


dropWhile' :: (a -> Bool) -> [a] -> [a]
dropWhile' f [] = []
dropWhile' f xss@(x:xs)
  | f x       = dropWhile' f xs
  | otherwise = xss

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "dropWhile'" $ do
    prop "is the same as dropWhile" $ \f' xs ->
      let f :: Int -> Bool
          f = Fun.apply f' in
        dropWhile' f xs == dropWhile f xs

  describe "takeWhile'" $ do
    prop "is the same as takeWhile" $ \f' xs ->
      let f :: Int -> Bool
          f = Fun.apply f' in
        takeWhile' f xs == takeWhile f xs

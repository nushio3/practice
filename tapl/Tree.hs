{-# LANGUAGE DeriveFunctor #-}
import Control.Applicative
import Control.Monad

data Tree a = Leaf a | Tuple [Tree a]
                       deriving (Eq,Ord,Show,Read,Functor)
zipTreeWith :: (a -> b -> c) -> Tree a -> Tree b -> Either String (Tree c)
zipTreeWith f ta tb = case (ta,tb) of
  (Leaf a0, Leaf b0)  -> return $ Leaf (f a0 b0)
  (Leaf a0, b0@(Tuple bs)) -> return $ fmap (f a0) b0
  (a0@(Tuple as), Leaf b0) -> return $ fmap (flip f b0) a0
  (Tuple as, Tuple bs) | length as /= length bs -> Left "Tuple size mismatch"
  (Tuple as, Tuple bs) -> Tuple <$> zipWithM (zipTreeWith f) as bs

main :: IO ()
main = do
  print testTree1
  print testTree2
  print $ zipTreeWith (\n xs -> concat $ replicate n xs)testTree1 testTree2

testTree1 :: Tree Int
testTree1 = Leaf 4 -- Tuple [Leaf 3, Tuple[Leaf 4, Leaf 1]]

testTree2 :: Tree String
testTree2 = Tuple [Leaf "hoge", Leaf "M"]

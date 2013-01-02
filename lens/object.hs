{-# LANGUAGE RankNTypes #-}
import           Control.Applicative hiding (empty)
import           Control.Lens
import qualified Data.Map as Map

type Obj = Map.Map String Double

member :: String -> Simple Traversal Obj Double
member key c2fd obj = case Map.lookup key obj of
  Just c  -> (\d -> Map.insert key d obj) <$> (c2fd c)
  Nothing -> (\d -> Map.insert key d obj) <$> (c2fd 20)

empty, x:: Obj
empty = Map.empty

x = empty & set (member "price") 120


main = do
  print empty
  print $ empty ^? member "price"
  print $ x ^? member "price"

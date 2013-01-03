{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE RankNTypes #-}
import           Control.Applicative hiding (empty)
import           Control.Lens
import           Data.List (intercalate)
import qualified Data.Map as Map
import           Text.Printf
import           Data.Dynamic
import           Data.Dynamic.Lens

newtype Obj = Obj { unObj :: Map.Map String Dynamic }
        deriving (Show)

data NotFound = NotFound
  deriving (Show, Typeable)


member :: String -> Simple Lens Obj Dynamic
member tag = lens gettr settr
  where
    gettr (Obj o) = case Map.lookup tag o of
      Just dx -> dx
      Nothing -> toDyn NotFound
    settr (Obj o) dx = Obj $ Map.insert tag dx o

empty,x,y :: Obj
empty = Obj Map.empty

x = empty & set (member "price") (toDyn (1::Int))

y = empty & set (member "price" . dynamic) (1::Int)

main = do
  print $ toDyn (1::Int)
  print empty
  print x
  let p :: Maybe Int
      p = x ^? (member "price" . dynamic)
  print p

  print y
  let p :: Maybe Int
      p = y ^? (member "price" . dynamic)
  print p


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

-- I want 'y' to behave the same as 'x'
x = empty & set (member "price") (toDyn (299::Int))
y = empty & set (member "price" . dynamic) (299::Int)

main = do
  print empty
  print x  -- Obj {unObj = fromList [("price",<<Int>>)]}
  let p :: Maybe Int
      p = x ^? (member "price" . dynamic)
  print p  -- Just 299

  print y -- Obj {unObj = fromList [("price",<<NotFound>>)]}
  let p :: Maybe Int
      p = y ^? (member "price" . dynamic)
  print p  -- Nothing


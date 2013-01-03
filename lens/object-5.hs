{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE RankNTypes #-}
import           Control.Applicative hiding (empty)
import           Control.Lens
import qualified Data.Map as Map
import           Text.Printf

newtype Object = Object { unObject :: Map.Map String Double }
        deriving (Show)

member :: String -> Simple Traversal Object Double
member tag d2ad obj@(Object kvp) =
 case Map.lookup tag kvp of
   Just d -> (\d' -> Object $ Map.insert tag d' kvp) <$> (d2ad d)
   Nothing  -> pure obj

memberWithDefault :: String -> (Object -> Maybe Double) -> Simple Traversal Object Double
memberWithDefault tag defFunc d2ad obj@(Object kvp) =
  case Map.lookup tag kvp of
    Just d -> go d
    Nothing  -> case defFunc obj of
      Just d -> go d
      Nothing -> pure obj
  where
       go d = (\d' -> Object $ Map.insert tag d' kvp) <$> (d2ad d)


mass, velocity, momentum ::  Simple Traversal Object Double
mass = member "mass"
velocity = memberWithDefault "velocity" $ \this -> do
  m <- this ^? mass
  mv <- this ^? momentum
  return (mv/m)

momentum = memberWithDefault "momentum" $ \this -> do
  m <- this ^? mass
  v <- this ^? velocity
  return (m*v)


insert :: String -> Double -> Object -> Object
insert tag d (Object kvp) = Object $Map.insert tag d kvp

empty, x, y :: Object
empty = Object Map.empty
x = empty & insert "mass" 30
          & insert "velocity" 40

y = empty & insert "mass" 50
          & insert "momentum" 1500

main = do
  print empty
  print x
  print $ x ^? velocity
  print $ x ^? momentum
  print $ y ^? velocity
  print $ y ^? momentum
  print $ empty ^? momentum

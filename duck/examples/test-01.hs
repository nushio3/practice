import Control.Lens
import Control.Monad
import Data.DuckTyped
import Data.DuckTyped.Flying
import Data.Maybe

x1,x2,x3,x4 :: Object
x1 = empty

x2 = x1 & speed .~ Just 120

x3 = x2 & sound .~ Just "quack! quack. quack? quack..."

x4 = x1 & over speed (fmap (*2)) -- accelerate duck!

main = do
  print x1
  print $ x1 ^. speed
  print x2
  print $ x2 ^. speed
  print $ x3 ^. sound
  let speeders :: [Object]
      speeders = do
        x <- [x1, x2, x3, x4]
        s <- maybeToList $ x ^. speed
        guard $ s < 130
        return x
  print speeders